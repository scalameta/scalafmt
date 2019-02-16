package org.scalafmt.dynamic

import com.geirsson.coursiersmall.CoursierSmall
import com.geirsson.coursiersmall.Dependency
import com.geirsson.coursiersmall.Repository
import com.geirsson.coursiersmall.ResolutionException
import com.geirsson.coursiersmall.Settings
import java.lang.reflect.InvocationTargetException
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

import com.typesafe.config.{ConfigException, ConfigFactory}
import org.scalafmt.interfaces._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

final case class ScalafmtDynamic(
    reporter: ScalafmtReporter,
    respectVersion: Boolean,
    respectExcludeFilters: Boolean,
    defaultVersion: String,
    fmts: mutable.Map[String, ScalafmtReflect]
) extends Scalafmt {
  override def clear(): Unit = {
    fmts.values.foreach(_.classLoader.close())
    fmts.clear()
  }
  def this() = this(
    ConsoleScalafmtReporter,
    true,
    true,
    BuildInfo.stable,
    TrieMap.empty[String, ScalafmtReflect]
  )

  override def withReporter(reporter: ScalafmtReporter): Scalafmt = {
    copy(reporter = reporter)
  }
  override def withRespectProjectFilters(
      respectExcludeFilters: Boolean
  ): Scalafmt = {
    copy(respectExcludeFilters = respectExcludeFilters)
  }
  override def withRespectVersion(respectVersion: Boolean): Scalafmt = {
    copy(respectVersion = respectVersion)
  }

  override def withDefaultVersion(defaultVersion: String): Scalafmt = {
    copy(defaultVersion = defaultVersion)
  }

  override def format(config: String, file: Path, code: String): String = {
    val configFile = Paths.get("stdin.conf")
    formatInternal(configFile, config, file, code)
  }

  override def format(config: Path, file: Path, code: String): String = {
    if (!Files.exists(config)) {
      reporter.error(config, "file does not exist")
      code
    } else {
      val configStr = readConfig(config)
      formatInternal(config, configStr, file, code)
    }
  }

  private def formatInternal(
      configFile: Path,
      configStr: String,
      file: Path,
      code: String
  ): String = {
    import ScalafmtDynamic.ConfigTextOps
    def report(e: Throwable): Unit = e match {
      case e: InvocationTargetException =>
        report(e.getCause)
      case _ =>
        reporter.error(file, e)
    }
    def tryFormat(reflect: ScalafmtReflect): String = {
      try {
        reflect.format(file, code)
      } catch {
        case VersionMismatch(_, _) =>
          fmts.remove(configStr.sha1).foreach(_.classLoader.close())
          formatInternal(configFile, configStr, file, code)
        case ScalafmtConfigException(msg) =>
          reporter.error(configFile, msg)
          code
        case NonFatal(e) =>
          report(e)
          code
      }
    }
    fmts.get(configStr.sha1) match {
      case Some(fmt) =>
        tryFormat(fmt)
      case None =>
        readVersion(configStr) match {
          case Some(version) =>
            loadScalafmt(configFile, configStr, version) match {
              case Some(fmt) =>
                tryFormat(fmt)
              case None =>
                code
            }
          case None =>
            reporter.missingVersion(configFile, defaultVersion)
            code
        }
    }
  }

  private def readVersion(config: String): Option[String] = {
    try {
      Some(ConfigFactory.parseString(config).getString("version"))
    } catch {
      case _: ConfigException.Missing if !respectVersion =>
        Some(defaultVersion)
      case NonFatal(_) =>
        None
    }
  }

  private def loadScalafmt(
      configPath: Path,
      configStr: String,
      version: String
  ): Option[ScalafmtReflect] = {
    def errorMessage = s"failed to resolve Scalafmt version '$version'"
    try {
      val scalaBinaryVersion =
        if (version.startsWith("0.")) "2.11"
        else "2.12"
      val scalaVersion =
        if (version.startsWith("0.")) BuildInfo.scala211
        else BuildInfo.scala
      val organization =
        if (version.startsWith("1") || version.startsWith("0") || version == "2.0.0-RC1") {
          "com.geirsson"
        } else {
          "org.scalameta"
        }
      val jars = CoursierSmall.fetch(
        new Settings()
          .withDependencies(
            List(
              new Dependency(
                organization,
                s"scalafmt-cli_$scalaBinaryVersion",
                version
              ),
              new Dependency(
                "org.scala-lang",
                "scala-reflect",
                scalaVersion
              )
            )
          )
          .withTtl(Some(Duration.Inf))
          .withWriter(reporter.downloadWriter())
          .withRepositories(
            List(
              Repository.MavenCentral,
              Repository.Ivy2Local,
              Repository.SonatypeReleases,
              Repository.SonatypeSnapshots
            )
          )
      )
      val urls = jars.iterator.map(_.toUri.toURL).toArray
      val classloader = new URLClassLoader(urls, null)
      val fmt = ScalafmtReflect(
        classloader,
        configPath,
        configStr,
        version,
        respectVersion,
        respectExcludeFilters,
        reporter
      )
      import ScalafmtDynamic.ConfigTextOps
      fmts(configStr.sha1) = fmt
      Some(fmt)
    } catch {
      case _: ResolutionException =>
        reporter.error(configPath, errorMessage)
        None
      case NonFatal(e) =>
        reporter.error(configPath, ScalafmtException(errorMessage, e))
        None
    }
  }

  private[this] def readConfig(configFile: Path): String = {
    new String(Files.readAllBytes(configFile), StandardCharsets.UTF_8)
  }
}

object ScalafmtDynamic {
  private[this] val md = MessageDigest.getInstance("SHA-1")
  implicit class ConfigTextOps(private val configStr: String) extends AnyVal {
    def sha1: String = {
      md.digest(configStr.getBytes("UTF-8")).map("%02x".format(_)).mkString
    }
  }
}
