package org.scalafmt.dynamic

import java.io.Closeable
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

import com.typesafe.config.{ConfigException, ConfigFactory}
import coursierapi.{MavenRepository, Repository}
import org.scalafmt.dynamic.ScalafmtDynamic.{FormatEval, FormatResult}
import org.scalafmt.dynamic.ScalafmtDynamicDownloader._
import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.dynamic.exceptions._
import org.scalafmt.dynamic.utils.ReentrantCache
import org.scalafmt.interfaces._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

final case class ScalafmtDynamic(
    reporter: ScalafmtReporter,
    repositories: List[Repository],
    respectExcludeFilters: Boolean,
    formatCache: ReentrantCache[ScalafmtVersion, FormatEval[ScalafmtReflect]],
    cacheConfigs: Boolean,
    configsCache: ReentrantCache[Path, FormatEval[
      (ScalafmtReflectConfig, FileTime)
    ]]
) extends Scalafmt
    with ScalafmtSessionFactory {

  def this() =
    this(
      ConsoleScalafmtReporter,
      Nil,
      true,
      ReentrantCache(),
      true,
      ReentrantCache()
    )

  override def clear(): Unit =
    formatCache.clear().foreach {
      _.foreach {
        _.right.foreach(_.classLoader match {
          case x: Closeable => x.close()
          case _ =>
        })
      }(ExecutionContext.global)
    }

  override def withReporter(reporter: ScalafmtReporter): ScalafmtDynamic =
    copy(reporter = reporter)

  override def withRespectProjectFilters(
      respectExcludeFilters: Boolean
  ): ScalafmtDynamic =
    copy(respectExcludeFilters = respectExcludeFilters)

  override def withRespectVersion(respectVersion: Boolean): ScalafmtDynamic =
    if (respectVersion) this
    else throw new ScalafmtInterfaceMethodDeprecated("withRespectVersion")

  override def withDefaultVersion(defaultVersion: String): ScalafmtDynamic =
    throw new ScalafmtInterfaceMethodDeprecated("withDefaultVersion")

  def withConfigCaching(cacheConfigs: Boolean): ScalafmtDynamic =
    copy(cacheConfigs = cacheConfigs)

  override def format(config: Path, file: Path, code: String): String = {
    formatDetailed(config, file, code) match {
      case Right(codeFormatted) =>
        codeFormatted
      case Left(error) =>
        reportError(file, error)
        if (error.isInstanceOf[ConfigError]) throw error
        code
    }
  }

  private def reportError(file: Path, error: ScalafmtDynamicError): Unit = {
    error match {
      case e: ConfigMissingVersion =>
        reporter.missingVersion(e.configPath, BuildInfo.stable)
      case e: ConfigError =>
        Option(e.getCause) match {
          case Some(cause) => reporter.error(e.configPath, e.getMessage, cause)
          case None => reporter.error(e.configPath, e.getMessage)
        }
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
      codeFormatted <- tryFormat(file, code, config)
    } yield codeFormatted
  }

  private[dynamic] def resolveConfig(
      configPath: Path
  ): Either[ScalafmtDynamicError, ScalafmtReflectConfig] = {
    if (!Files.exists(configPath)) {
      Left(new ConfigDoesNotExist(configPath))
    } else if (cacheConfigs) {
      val currentTimestamp: FileTime = Files.getLastModifiedTime(configPath)
      configsCache
        .getOrAddToCache(
          configPath,
          _.fold(_ => true, _._2.compareTo(currentTimestamp) != 0)
        ) { () =>
          resolveConfigWithScalafmt(configPath).map { config =>
            reporter.parsedConfig(configPath, config.getVersion.toString)
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
      version <- readVersion(configPath)
      // can't use current build directly, -dynamic doesn't include -core
      fmtReflect <- resolveFormatter(configPath, version)
      config <- fmtReflect.parseConfig(configPath).toEither.left.map {
        case ex: ScalafmtDynamicError => ex
        case ex => new UnknownConfigError(configPath, ex)
      }
    } yield config
  }

  private def resolveFormatter(
      configPath: Path,
      version: ScalafmtVersion
  ): FormatEval[ScalafmtReflect] = {
    formatCache.getOrAddToCache(version) { () =>
      val writer = reporter.downloadOutputStreamWriter()
      new ScalafmtDynamicDownloader(writer, repositories)
        .download(version)
        .left
        .map {
          case _: DownloadResolutionError =>
            new CannotDownload(configPath, version)
          case DownloadUnknownError(cause) =>
            new CannotDownload(configPath, version, cause)
        }
        .flatMap(resolveClassPath(configPath))
    }
  }

  private def resolveClassPath(configPath: Path)(
      downloadSuccess: DownloadSuccess
  ): FormatEval[ScalafmtReflect] = {
    val DownloadSuccess(version, urls) = downloadSuccess
    Try {
      val classloader = new URLClassLoader(urls.toArray, null)
      ScalafmtReflect(classloader, version)
    }.toEither.left.map {
      case e: ReflectiveOperationException =>
        new CorruptedClassPath(configPath, version, urls, e)
      case e =>
        UnknownError(e)
    }
  }

  private def tryFormat(
      file: Path,
      code: String,
      config: ScalafmtReflectConfig
  ): FormatResult =
    if (respectExcludeFilters && !config.isIncludedInProject(file)) {
      reporter.excluded(file)
      Right(code)
    } else
      tryForceFormat(file, code, config)

  private def tryForceFormat(
      file: Path,
      code: String,
      config: ScalafmtReflectConfig
  ): FormatResult = {
    Try {
      val filename = file.toString
      val configWithDialect: ScalafmtReflectConfig =
        if (
          config.getVersion < ScalafmtVersion(2, 6, 3) &&
          (filename.endsWith(".sbt") || filename.endsWith(".sc") ||
            filename.endsWith(".md"))
        ) {
          config.withSbtDialect
        } else {
          config
        }
      configWithDialect.format(code, Some(file))
    }.toEither.left.map {
      case ReflectionException(e) => UnknownError(e)
      case e => UnknownError(e)
    }
  }

  private def readVersion(config: Path): FormatEval[ScalafmtVersion] = {
    Try {
      ConfigFactory.parseFile(config.toFile).getString("version")
    } match {
      case Failure(e: ConfigException.Parse) =>
        Left(new ConfigParseError(config, e.getMessage))
      case Failure(_: ConfigException.Missing) =>
        Left(new ConfigMissingVersion(config))
      case Failure(e) =>
        Left(new UnknownConfigError(config, e))
      case Success(v) =>
        ScalafmtVersion.parse(v).toRight(new ConfigInvalidVersion(config, v))
    }
  }

  override def withMavenRepositories(repositories: String*): Scalafmt =
    copy(repositories = repositories.map(MavenRepository.of).toList)

  override def createSession(config: Path): ScalafmtSession =
    resolveConfig(config).fold(
      error => { reportError(config, error); throw error },
      config => new MySession(config)
    )

  private class MySession(cfg: ScalafmtReflectConfig) extends ScalafmtSession {
    override def format(file: Path, code: String): String =
      tryFormat(file, code, cfg).fold(
        error => { reportError(file, error); code },
        formatted => formatted
      )
    override def matchesProjectFilters(file: Path): Boolean =
      cfg.isIncludedInProject(file)
  }

}

object ScalafmtDynamic {
  type FormatResult = Either[ScalafmtDynamicError, String]
  private type FormatEval[T] = Either[ScalafmtDynamicError, T]
}
