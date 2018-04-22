package org.scalafmt.config

import scala.language.reflectiveCalls

import metaconfig._

import java.io.File

import metaconfig.Conf
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.Ok
import org.scalafmt.config.PlatformConfig._

object Config {

  def fromInput(input: Input, path: Option[String]): Configured[Conf] = {
    val configured = implicitly[MetaconfigParser].fromInput(input)
    path match {
      case Some(x) => ConfDynamic(configured).selectDynamic(x).asConf
      case None => configured
    }
  }

  def fromHoconString(
      string: String,
      path: Option[String] = None): Configured[ScalafmtConfig] =
    fromConf(fromInput(Input.String(string), path))

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconFile(
      file: File,
      path: Option[String] = None): Configured[ScalafmtConfig] =
    fromConf(fromInput(Input.File(file), path))

  def fromConf(
      conf: Configured[Conf],
      path: Option[String] = None): Configured[ScalafmtConfig] =
    conf.andThen { baseConf =>
      val conf = path match {
        case None => Ok(baseConf)
        case Some(p) =>
          baseConf match {
            case Conf.Obj(values) =>
              values
                .collectFirst { case (`p`, value) => Ok(value) }
                .getOrElse(
                  ConfError.message(s"Config $baseConf has no field $p").notOk)
            case x =>
              ConfError.typeMismatch("Conf.Obj", x).notOk
          }
      }
      conf.andThen { conf =>
        ScalafmtConfig
          .configReader(ScalafmtConfig.default)
          .read(conf)
      }
    }

}
