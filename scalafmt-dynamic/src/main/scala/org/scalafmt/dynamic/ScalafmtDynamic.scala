package org.scalafmt.dynamic

import java.net.URLClassLoader
import java.nio.file.{Files, Path}

import com.typesafe.config.{ConfigException, ConfigFactory}
import org.scalafmt.dynamic.ScalafmtDynamic.FormatResult
import org.scalafmt.dynamic.ScalafmtDynamicDownloader.{DownloadResolutionError, DownloadSuccess, DownloadUnknownError}
import org.scalafmt.dynamic.exceptions._
import org.scalafmt.dynamic.utils.ConsoleScalafmtReporter
import org.scalafmt.interfaces._

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Try
import scala.util.control.NonFatal

final case class ScalafmtDynamic(
    reporter: ScalafmtReporter,
    respectVersion: Boolean,
    respectExcludeFilters: Boolean,
    defaultVersion: String,
    cacheConfig: Boolean,
    fmts: mutable.Map[Path, ScalafmtReflect]
) extends Scalafmt {

  def this() = this(
    ConsoleScalafmtReporter,
    true,
    true,
    BuildInfo.stable,
    true,
    TrieMap.empty[Path, ScalafmtReflect]
  )

  override def clear(): Unit = {
    fmts.values.foreach(_.classLoader.close())
    fmts.clear()
  }

  override def withReporter(reporter: ScalafmtReporter): Scalafmt =
    copy(reporter = reporter)

  override def withRespectProjectFilters(
      respectExcludeFilters: Boolean): Scalafmt =
    copy(respectExcludeFilters = respectExcludeFilters)

  override def withRespectVersion(respectVersion: Boolean): Scalafmt =
    copy(respectVersion = respectVersion)

  override def withDefaultVersion(defaultVersion: String): Scalafmt =
    copy(defaultVersion = defaultVersion)

  def withConfigCaching(cacheConfig: Boolean): Scalafmt =
    copy(cacheConfig = cacheConfig)

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
    error match {
      case ScalafmtDynamicError.ConfigParseError(configPath, cause) =>
        reporter.error(configPath, cause.getMessage)
      case ScalafmtDynamicError.ConfigDoesNotExist(configPath) =>
        reporter.error(configPath, "file does not exist")
      case ScalafmtDynamicError.ConfigMissingVersion(configPath) =>
        reporter.missingVersion(configPath, defaultVersion)
      case ScalafmtDynamicError.CannotDownload(configPath, version, cause) =>
        val message = s"failed to resolve Scalafmt version '$version'"
        cause match {
          case Some(e) => reporter.error(configPath, message, e)
          case None => reporter.error(configPath, message)
        }
      case ScalafmtDynamicError.CorruptedClassPath(configPath, version, _, cause) =>
        reporter.error(configPath, s"scalafmt version $version classpath is corrupted", cause)
      case ScalafmtDynamicError.UnknownError(cause) =>
        reporter.error(file, cause)
    }
  }

  def formatDetailed(config: Path, file: Path, code: String): FormatResult = {
    def tryFormat(reflect: ScalafmtReflect): FormatResult = {
      Try(reflect.format(file, code)).toEither.left.flatMap {
        case VersionMismatch(_, _) =>
          fmts.remove(config).foreach(_.classLoader.close())
          formatDetailed(config, file, code)
        case ex: ScalafmtConfigException =>
          Left(ScalafmtDynamicError.ConfigParseError(config, ex))
        case ReflectionException(e) =>
          Left(ScalafmtDynamicError.UnknownError(e))
        case NonFatal(e) =>
          Left(ScalafmtDynamicError.UnknownError(e))
      }
    }

    def downloadAndFormat: FormatResult =
      for {
        _ <- Option(config).filter(conf => Files.exists(conf)).toRight {
          ScalafmtDynamicError.ConfigDoesNotExist(config)
        }
        version <- readVersion(config).toRight {
          ScalafmtDynamicError.ConfigMissingVersion(config)
        }
        fmtReflect <- downloadAndResolve(config, version)
        codeFormatted <- {
          fmts(config) = fmtReflect
          tryFormat(fmtReflect)
        }
      } yield codeFormatted

    fmts
      .get(config)
      .map(tryFormat)
      .getOrElse(downloadAndFormat)
  }

  private def downloadAndResolve(config: Path, version: String): Either[ScalafmtDynamicError, ScalafmtReflect] = {
    val downloader = new ScalafmtDynamicDownloader(reporter.downloadWriter())
    downloader.download(version)
      .left.map {
        case f@DownloadResolutionError(_, _) =>
          ScalafmtDynamicError.CannotDownload(config, f.version, None)
        case f@DownloadUnknownError(_, _) =>
          ScalafmtDynamicError.CannotDownload(config, f.version, Option(f.cause))
      }
      .flatMap { case DownloadSuccess(_, urls) =>
        Try {
          val classloader = new URLClassLoader(urls.toArray, null)
          ScalafmtReflect(
            classloader,
            config,
            cacheConfig,
            version,
            respectVersion,
            respectExcludeFilters,
            reporter
          )
        }.toEither.left.map {
          case e: ReflectiveOperationException =>
            ScalafmtDynamicError.CorruptedClassPath(config, version, urls, e)
          case e =>
            ScalafmtDynamicError.UnknownError(e)
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
}

object ScalafmtDynamic {
  type FormatResult = Either[ScalafmtDynamicError, String]
}
