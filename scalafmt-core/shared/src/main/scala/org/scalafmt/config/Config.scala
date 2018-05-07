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

  def toHocon[T: ConfEncoder](value: T): String = {
    val conf = ConfEncoder[T].write(value)
    val out = new StringBuilder
    def loop(c: Conf): Unit = c match {
      case Conf.Null() => out.append("null").append("\n")
      case Conf.Num(num) => out.append(num).append("\n")
      case Conf.Str(str) => out.append('"').append(str).append('"').append("\n")
      case Conf.Bool(bool) => out.append(bool).append("\n")
      case Conf.Lst(lst) =>
        out.append("[\n")
        lst.foreach { elem =>
          out.append("  ")
          loop(elem)
        }
        out.append("]\n")
      case Conf.Obj(obj) =>
        obj.foreach {
          case (key, value) =>
            out.append(key).append(" = ")
            loop(value)
        }
    }

    def flatten(c: Conf): Conf = c match {
      case Conf.Obj(obj) =>
        val flattened = obj.flatMap {
          case (key, Conf.Obj(nested)) =>
            nested.map {
              case (k, v) =>
                s"$key.$k" -> flatten(v)
            }
          case x => x :: Nil
        }
        Conf.Obj(flattened)
      case x => x
    }
    loop(flatten(conf))

    out.toString()
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
      val next = path match {
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
      ScalafmtConfig
        .configReader(ScalafmtConfig.default)
        .read(next)
    }

}
