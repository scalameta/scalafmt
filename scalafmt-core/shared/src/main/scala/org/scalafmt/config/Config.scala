package org.scalafmt.config

import scala.language.reflectiveCalls
import metaconfig._
import java.io.File
import metaconfig.Conf
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.Ok
import org.scalafmt.config.PlatformConfig._

// NOTE: these methods are intended for internal usage and are subject to
// binary and source breaking changes between any release. For a stable API
// use org.scalafmt.Scalafmt. Documentation on using scalafmt as a library
// can be seen here https://scalameta.org/scalafmt/#Standalonelibrary
object Config {

  def fromInput(input: Input, path: Option[String]): Configured[Conf] = {
    val configured = implicitly[MetaconfigParser].fromInput(input)
    path match {
      case Some(x) => ConfDynamic(configured).selectDynamic(x).asConf
      case None => configured
    }
  }

  def fromHoconString(string: String): Configured[ScalafmtConfig] =
    fromHoconString(string, None)

  def fromHoconString(
      string: String,
      path: Option[String]
  ): Configured[ScalafmtConfig] =
    fromHoconString(string, path, ScalafmtConfig.default)

  def fromHoconString(
      string: String,
      path: Option[String],
      default: ScalafmtConfig
  ): Configured[ScalafmtConfig] =
    fromConf(fromInput(Input.String(string), path), default = default)

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconFile(
      file: File,
      path: Option[String] = None,
      default: ScalafmtConfig = ScalafmtConfig.default
  ): Configured[ScalafmtConfig] =
    fromConf(fromInput(Input.File(file), path), default = default)

  def fromConf(
      conf: Configured[Conf],
      path: Option[String] = None,
      default: ScalafmtConfig = ScalafmtConfig.default
  ): Configured[ScalafmtConfig] =
    conf.andThen { baseConf =>
      val next = path match {
        case None => Ok(baseConf)
        case Some(p) =>
          baseConf match {
            case Conf.Obj(values) =>
              values
                .collectFirst { case (`p`, value) => Ok(value) }
                .getOrElse(
                  ConfError.message(s"Config $baseConf has no field $p").notOk
                )
            case x =>
              ConfError.typeMismatch("Conf.Obj", x).notOk
          }
      }
      ScalafmtConfig.configReader(default).read(next)
    }

}
