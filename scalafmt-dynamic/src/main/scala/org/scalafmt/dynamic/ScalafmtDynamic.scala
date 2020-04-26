package org.scalafmt.dynamic

import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

import com.typesafe.config.{ConfigException, ConfigFactory}
import coursierapi.{MavenRepository, Repository}
import org.scalafmt.dynamic.ScalafmtDynamic.{FormatEval, FormatResult}
import org.scalafmt.dynamic.ScalafmtDynamicDownloader._
import org.scalafmt.dynamic.exceptions._
import org.scalafmt.dynamic.utils.ReentrantCache
import org.scalafmt.interfaces._

import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.util.control.NonFatal

final case class ScalafmtDynamic(
    reporter: ScalafmtReporter,
    repositories: List[Repository],
    respectVersion: Boolean,
    respectExcludeFilters: Boolean,
    defaultVersion: String,
    formatCache: ReentrantCache[String, FormatEval[ScalafmtReflect]],
    cacheConfigs: Boolean,
    configsCache: ReentrantCache[Path, FormatEval[
      (ScalafmtReflectConfig, FileTime)
    ]]
) extends ScalafmtSessionFactory {

  def this() =
    this(
      ConsoleScalafmtReporter,
      Nil,
      true,
      true,
      BuildInfo.stable,
      ReentrantCache(),
      true,
      ReentrantCache()
    )

  override def clear(): Unit =
    formatCache
      .clear()
      .foreach(
        _.foreach(_.right.foreach(_.classLoader.close()))(
          ExecutionContext.global
        )
      )

  override def withReporter(reporter: ScalafmtReporter): ScalafmtDynamic =
    copy(reporter = reporter)

  override def withRespectProjectFilters(
      respectExcludeFilters: Boolean
  ): ScalafmtDynamic =
    copy(respectExcludeFilters = respectExcludeFilters)

  override def withRespectVersion(respectVersion: Boolean): ScalafmtDynamic =
    copy(respectVersion = respectVersion)

  override def withDefaultVersion(defaultVersion: String): ScalafmtDynamic =
    copy(defaultVersion = defaultVersion)

  def withConfigCaching(cacheConfigs: Boolean): ScalafmtDynamic =
    copy(cacheConfigs = cacheConfigs)

  override def format(config: Path, file: Path, code: String): String = {
    formatDetailed(config, file, code) match {
      case Right(codeFormatted) =>
        codeFormatted
      case Left(error) =>
        reportError(file, error)
        code
    }
  }

  private def reportError(file: Path, error: ScalafmtDynamicError): Unit = {
    import ScalafmtDynamicError._
    error match {
      case ConfigParseError(configPath, cause) =>
        reporter.error(configPath, cause.getMessage)
      case ConfigDoesNotExist(configPath) =>
        reporter.error(configPath, "file does not exist")
      case ConfigMissingVersion(configPath) =>
        reporter.missingVersion(configPath, defaultVersion)
      case CannotDownload(configPath, version, cause) =>
        val message = s"failed to resolve Scalafmt version '$version'"
        cause match {
          case Some(e) => reporter.error(configPath, message, e)
          case None => reporter.error(configPath, message)
        }
      case CorruptedClassPath(configPath, version, _, cause) =>
        val message = s"scalafmt version $version classpath is corrupted"
        reporter.error(configPath, message, cause)
      case UnknownError(cause) =>
        reporter.error(file, cause)
    }
  }

  def formatDetailed(
      configPath: Path,
      file: Path,
      code: String
  ): FormatResult = {
    for {
      config <- resolveConfig(configPath)
      codeFormatted <- tryFormat(file, code, config.fmtReflect, config)
    } yield codeFormatted
  }

  private def resolveConfig(
      configPath: Path
  ): Either[ScalafmtDynamicError, ScalafmtReflectConfig] = {
    if (!Files.exists(configPath)) {
      Left(ScalafmtDynamicError.ConfigDoesNotExist(configPath))
    } else if (cacheConfigs) {
      val currentTimestamp: FileTime = Files.getLastModifiedTime(configPath)
      configsCache
        .getOrAddToCache(
          configPath,
          _.fold(_ => true, _._2.compareTo(currentTimestamp) != 0)
        ) { () =>
          resolveConfigWithScalafmt(configPath).map { config =>
            reporter.parsedConfig(configPath, config.version)
            (config, currentTimestamp)
          }
        }
        .map(_._1)
    } else {
      resolveConfigWithScalafmt(configPath)
    }
  }
  private def resolveConfigWithScalafmt(
      configPath: Path
  ): FormatEval[ScalafmtReflectConfig] = {
    for {
      version <- readVersion(configPath).toRight(
        ScalafmtDynamicError.ConfigMissingVersion(configPath)
      )
      fmtReflect <- resolveFormatter(configPath, version)
      config <- parseConfig(configPath, fmtReflect)
    } yield config
  }

  private def parseConfig(
      configPath: Path,
      fmtReflect: ScalafmtReflect
  ): FormatEval[ScalafmtReflectConfig] = {
    Try(fmtReflect.parseConfig(configPath)).toEither.left.map {
      case ex: ScalafmtConfigException =>
        ScalafmtDynamicError.ConfigParseError(configPath, ex)
      case ex =>
        ScalafmtDynamicError.UnknownError(ex)
    }
  }

  private def resolveFormatter(
      configPath: Path,
      version: String
  ): FormatEval[ScalafmtReflect] = {
    formatCache.getOrAddToCache(version) { () =>
      val writer = reporter.downloadOutputStreamWriter()
      val downloader = new ScalafmtDynamicDownloader(writer, repositories)
      downloader
        .download(version)
        .left
        .map {
          case DownloadResolutionError(v, _) =>
            ScalafmtDynamicError.CannotDownload(configPath, v, None)
          case DownloadUnknownError(v, cause) =>
            ScalafmtDynamicError.CannotDownload(configPath, v, Option(cause))
          case InvalidVersionError(v, cause) =>
            ScalafmtDynamicError.CannotDownload(configPath, v, Option(cause))
        }
        .flatMap(resolveClassPath(configPath, _))
    }
  }

  private def resolveClassPath(
      configPath: Path,
      downloadSuccess: DownloadSuccess
  ): FormatEval[ScalafmtReflect] = {
    val DownloadSuccess(version, urls) = downloadSuccess
    Try {
      val classloader = new URLClassLoader(urls.toArray, null)
      ScalafmtReflect(classloader, version, respectVersion)
    }.toEither.left.map {
      case e: ReflectiveOperationException =>
        ScalafmtDynamicError.CorruptedClassPath(configPath, version, urls, e)
      case e =>
        ScalafmtDynamicError.UnknownError(e)
    }
  }

  private def tryFormat(
      file: Path,
      code: String,
      reflect: ScalafmtReflect,
      config: ScalafmtReflectConfig
  ): FormatResult = {
    Try {
      val filename = file.toString
      val configWithDialect: ScalafmtReflectConfig =
        if (filename.endsWith(".sbt") || filename.endsWith(".sc")) {
          config.withSbtDialect
        } else {
          config
        }
      if (isIgnoredFile(filename, configWithDialect)) {
        reporter.excluded(file)
        code
      } else {
        reflect.format(code, configWithDialect, Some(file))
      }
    }.toEither.left.map {
      case ReflectionException(e) => ScalafmtDynamicError.UnknownError(e)
      case e => ScalafmtDynamicError.UnknownError(e)
    }
  }

  private def isIgnoredFile(
      filename: String,
      config: ScalafmtReflectConfig
  ): Boolean = {
    respectExcludeFilters && !config.isIncludedInProject(filename)
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

  override def withMavenRepositories(repositories: String*): Scalafmt =
    copy(repositories = repositories.map(MavenRepository.of).toList)

  override def createSession(config: Path): ScalafmtSession =
    resolveConfig(config).fold(
      error => { reportError(config, error); null },
      config => new MySession(config)
    )

  private class MySession(cfg: ScalafmtReflectConfig) extends ScalafmtSession {
    override def format(file: Path, code: String): String =
      tryFormat(file, code, cfg.fmtReflect, cfg).fold(
        error => { reportError(file, error); code },
        formatted => formatted
      )
    override def matchesProjectFilters(file: Path): Boolean =
      cfg.isIncludedInProject(file.toString)
  }

}

object ScalafmtDynamic {
  type FormatResult = Either[ScalafmtDynamicError, String]
  private type FormatEval[T] = Either[ScalafmtDynamicError, T]
}
