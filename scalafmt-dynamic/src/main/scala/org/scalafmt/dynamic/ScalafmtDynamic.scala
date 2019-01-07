package org.scalafmt.dynamic

import com.geirsson.coursiersmall.CoursierSmall
import com.geirsson.coursiersmall.Dependency
import com.geirsson.coursiersmall.Repository
import com.geirsson.coursiersmall.ResolutionException
import com.geirsson.coursiersmall.Settings
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigFactory
import java.io.PrintWriter
import java.io.Writer
import java.lang.reflect.InvocationTargetException
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
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
    writer: Writer,
    fmts: mutable.Map[Path, ScalafmtReflect]
) extends Scalafmt {
  override def clear(): Unit = {
    fmts.values.foreach(_.classLoader.close())
    fmts.clear()
  }
  def this() = this(
    ConsoleScalafmtReporter,
    true,
    true,
    BuildInfo.version,
    new PrintWriter(System.err),
    TrieMap.empty[Path, ScalafmtReflect]
  )

  override def withDownloadWriter(writer: Writer): Scalafmt = {
    copy(writer = writer)
  }
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
  def report(file: Path, e: Throwable): Unit = {
    if (e.isInstanceOf[InvocationTargetException]) {
      report(file, e.getCause)
    } else {
      e.getClass.getName match {
        case "scala.meta.parsers.ParseException" |
            "scala.meta.parsers.TokenizeException" =>
          val msg = e.toString
          if (msg.contains(file.toString)) {
            reporter.error(msg)
          } else {
            reporter.error(file, e.toString)
          }
        case _ =>
          reporter.error(file, e)
      }
    }
  }
  override def format(config: Path, file: Path, code: String): String = {
    def tryFormat(reflect: ScalafmtReflect): String = {
      try {
        reflect.format(file, code)
      } catch {
        case VersionMismatch(_, _) =>
          fmts.remove(config)
          format(config, file, code)
        case ScalafmtConfigException(msg) =>
          reporter.error(config, msg)
          code
        case NonFatal(e) =>
          report(file, e)
          code
      }
    }
    fmts.get(config) match {
      case Some(fmt) =>
        tryFormat(fmt)
      case None =>
        if (!Files.exists(config)) {
          reporter.error(config, "file does not exist")
          code
        } else {
          readVersion(config) match {
            case Some(version) =>
              loadScalafmt(config, version) match {
                case Some(fmt) =>
                  fmts(config) = fmt
                  tryFormat(fmt)
                case None =>
                  code
              }
            case None =>
              reporter.error(
                config,
                s"missing setting 'version'. " +
                  s"To fix this problem, add the following line to .scalafmt.conf: 'version=$defaultVersion'."
              )
              code
          }
        }
    }
  }

  private def readVersion(config: Path): Option[String] = {
    try {
      Some(ConfigFactory.parseFile(config.toFile).getString("version"))
    } catch {
      case _: ConfigException.Missing if !respectVersion =>
        Some(defaultVersion)
      case NonFatal(_) =>
        None
    }
  }

  private def loadScalafmt(
      config: Path,
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
      val jars = CoursierSmall.fetch(
        new Settings()
          .withDependencies(
            List(
              new Dependency(
                "com.geirsson",
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
          .withWriter(writer)
          .withRepositories(
            new Settings().repositories ++ List(
              Repository.SonatypeSnapshots
            )
          )
      )
      val urls = jars.iterator.map(_.toUri.toURL).toArray
      val classloader = new URLClassLoader(urls, null)
      val fmt = ScalafmtReflect(
        classloader,
        config,
        version,
        respectVersion,
        respectExcludeFilters,
        reporter
      )
      fmts(config) = fmt
      Some(fmt)
    } catch {
      case _: ResolutionException =>
        reporter.error(errorMessage)
        None
      case NonFatal(e) =>
        reporter.error(config, ScalafmtException(errorMessage, e))
        None
    }
  }

}
