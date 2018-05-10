package org.scalafmt.config

import scala.language.reflectiveCalls
import metaconfig._
import java.io.File
import metaconfig.Conf
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.Ok
import org.scalafmt.config.PlatformConfig._
import org.typelevel.paiges.Doc._
import org.typelevel.paiges.Doc

object Config {

  def fromInput(input: Input, path: Option[String]): Configured[Conf] = {
    val configured = implicitly[MetaconfigParser].fromInput(input)
    path match {
      case Some(x) => ConfDynamic(configured).selectDynamic(x).asConf
      case None => configured
    }
  }

  def quote(key: String): String =
    if (key.indexOf('.') < 0) key
    else "\"" + key + "\""

  val isForbiddenCharacter = Set[Char](
    '$', '"', '{', '}', '[', ']', ':', '=', ',', '+', '#', '`', '^', '?', '!',
    '@', '*', '&', ' ', '\\'
  )
  val quote = char('"')
  def quoteKey(key: String): Doc =
    if (key.indexOf('.') < 0) text(key)
    else quote + text(key) + quote

  // Spec is here:
  // https://github.com/lightbend/config/blob/master/HOCON.md#unquoted-strings
  // but this method is conservative and quotes if the string contains non-letter characters
  def needsQuote(str: String): Boolean =
    str.isEmpty ||
      str.startsWith("true") ||
      str.startsWith("false") ||
      str.startsWith("null") ||
      str.exists(!_.isLetter)

  def quoteString(str: String): Doc =
    if (needsQuote(str)) quote + text(str) + quote
    else text(str)

  def wrap(open: Char, close: Char, doc: Doc): Doc = {
    (char(open) + line + doc).nested(2) + line + char(close)
  }

  def toHocon[T: ConfEncoder](value: T): Doc = {
    toHocon(ConfEncoder[T].write(value))
  }
  def toHocon(conf: Conf): Doc = {
    def loop(c: Conf): Doc = {
      c match {
        case Conf.Null() => text("null")
        case Conf.Num(num) => str(num)
        case Conf.Str(str) => quoteString(str)
        case Conf.Bool(bool) => str(bool)
        case Conf.Lst(lst) =>
          if (lst.isEmpty) text("[]")
          else {
            val parts = intercalate(line, lst.map {
              case c: Conf.Obj =>
                wrap('{', '}', loop(c))
              case x => loop(x)
            })
            wrap('[', ']', parts)
          }
        case Conf.Obj(obj) =>
          intercalate(line, obj.map {
            case (k, v) =>
              text(k) + text(" = ") + loop(v)
          })
//            "[" +: (parts :+ " ]").nested(2)
//            out.append("[\n")
//            lst.foreach { elem =>
//              indent(nesting + 2)
//              elem match {
//                case _: Conf.Obj =>
//                  out.append('{')
//                  newline()
//                  loop(elem, nesting + 4)
//                  indent(nesting + 2)
//                  out.append('}')
//                case _ =>
//                  loop(elem, nesting + 2)
//              }
//              newline()
//            }
//            out.append("]")
//          }
//        case Conf.Obj(obj) =>
//          if (obj.isEmpty) out.append("{}")
//          else {
//            obj.foreach {
//              case (key, value) =>
//                indent(nesting)
//                out.append(key).append(" = ")
//                loop(value, nesting + 2)
//                newline()
//            }
//          }
      }
    }

    loop(flatten(conf))
  }

  def flatten(c: Conf): Conf = c match {
    case Conf.Obj(obj) =>
      val flattened = obj.map {
        case (k, v) => (k, flatten(v))
      }
      val next = flattened.flatMap {
        case (key, Conf.Obj(nested)) =>
          nested.map {
            case (k, v) => s"${quote(key)}.$k" -> v
          }
        case (key, value) => (quote(key), value) :: Nil
      }
      Conf.Obj(next)
    case Conf.Lst(lst) =>
      Conf.Lst(lst.map(flatten))
    case x => x
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
