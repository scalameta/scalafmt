package org.scalafmt.dynamic

import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.dynamic.utils.ReentrantCache

import java.io.FileNotFoundException
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}

import scala.util.{Failure, Success, Try}

import com.typesafe.config.{ConfigException, ConfigFactory}

trait ScalafmtConfigLoader {

  def load(
      configPath: Path,
      properties: ScalafmtProperties,
      moduleLoader: ScalafmtModuleLoader,
  ): FormatEval[ScalafmtReflectConfig]

}

object ScalafmtConfigLoader extends ScalafmtConfigLoader {

  override def load(
      configPath: Path,
      properties: ScalafmtProperties,
      moduleLoader: ScalafmtModuleLoader,
  ): FormatEval[ScalafmtReflectConfig] = for {
    version <- readVersion(configPath)
    // can't use current build directly, -dynamic doesn't include -core
    loaded <- moduleLoader.load(configPath, version, properties)
    config <- loaded.parseConfig(configPath).toEither.left.map {
      case ex: ScalafmtDynamicError => ex
      case ex => new UnknownConfigError(configPath, ex)
    }
  } yield {
    properties.reporter.parsedConfig(configPath, config.getVersion.toString)
    config
  }

  private def readVersion(config: Path): FormatEval[ScalafmtVersion] =
    Try(ConfigFactory.parseFile(config.toFile).getString("version")) match {
      case Failure(e: ConfigException.IO)
          if e.getCause.isInstanceOf[FileNotFoundException] =>
        Left(new ConfigDoesNotExist(config, e))
      case Failure(e: ConfigException.Parse) =>
        Left(new ConfigParseError(config, e.getMessage, e.getCause))
      case Failure(e: ConfigException.Missing) =>
        Left(new ConfigMissingVersion(config, e.getCause))
      case Failure(e) => Left(new UnknownConfigError(config, e))
      case Success(v) => ScalafmtVersion.parse(v)
          .toRight(new ConfigInvalidVersion(config, v))
    }

  class CachedProxy(loader: ScalafmtConfigLoader) extends ScalafmtConfigLoader {
    private[dynamic] type Value = FormatEval[(ScalafmtReflectConfig, FileTime)]
    private[dynamic] val cache: ReentrantCache[Path, Value] = ReentrantCache()

    override def load(
        configPath: Path,
        properties: ScalafmtProperties,
        moduleLoader: ScalafmtModuleLoader,
    ): FormatEval[ScalafmtReflectConfig] =
      Try(Files.getLastModifiedTime(configPath)).fold(
        err => Left(new ConfigDoesNotExist(configPath, err)),
        currentTimestamp => {
          def evict(value: Value) = value
            .fold(_ => true, _._2.compareTo(currentTimestamp) != 0)
          def load() = loader.load(configPath, properties, moduleLoader)
            .map((_, currentTimestamp))
          cache.getOrAddToCache(configPath, evict)(load).map(_._1)
        },
      )
  }

}
