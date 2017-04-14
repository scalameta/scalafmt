package org.scalafmt.config

import scala.language.reflectiveCalls

import scala.meta.internal.parsers.ScalametaParser

import java.io.File

import metaconfig.Conf
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.Ok
import metaconfig.HasFields
import metaconfig.String2AnyMap
import metaconfig.typesafeconfig.TypesafeConfig2Class
import org.scalameta.logger

object Config {

  def toHocon(any: Any): Seq[String] = any match {
    case String2AnyMap(m) =>
      m.flatMap {
        case (k, v) =>
          toHocon(v).map { x =>
            if (x.startsWith(" ")) s"$k$x"
            else s"$k.$x"
          }
      }.toSeq
    case x: HasFields => toHocon(x.fields)
    case x: Traversable[_] =>
      Seq(
        x.flatMap(toHocon)
          .map(_.stripPrefix(" = "))
          .mkString(" = [", "\n  ", "]"))
    case x =>
      val str = s"$x"
      val output =
        if (str.headOption.exists(!_.isLetterOrDigit)) s""""$str""""
        else str
      Seq(s" = $output")
  }

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconString(
      string: String,
      path: Option[String] = None): Configured[ScalafmtConfig] =
    fromConf(TypesafeConfig2Class.gimmeConfFromString(string), path)

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconFile(file: File,
                    path: Option[String] = None): Configured[ScalafmtConfig] =
    fromConf(TypesafeConfig2Class.gimmeConfFromFile(file), path)

  def fromConf(conf: Configured[Conf],
               path: Option[String] = None): Configured[ScalafmtConfig] =
    for {
      baseConf <- conf
      conf <- {
        path match {
          case None => Ok(baseConf)
          case Some(p) =>
            baseConf match {
              case Conf.Obj(values) =>
                values
                  .collectFirst { case (`p`, value) => Ok(value) }
                  .getOrElse(
                    ConfError.msg(s"Config $baseConf has no field $p").notOk)
              case x =>
                ConfError.typeMismatch("Conf.Obj", x).notOk
            }
        }
      }
      scalafmtConfig <- ScalafmtConfig
        .configReader(ScalafmtConfig.default)
        .read(conf)
    } yield scalafmtConfig

}
