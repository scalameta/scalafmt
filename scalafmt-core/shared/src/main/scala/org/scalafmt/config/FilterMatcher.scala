package org.scalafmt.config

import org.scalafmt.sysops.AbsoluteFile

import scala.util.matching.Regex

import metaconfig._

case class FilterMatcher(include: Regex, exclude: Regex) {
  def matchesFile(file: AbsoluteFile): Boolean = matches(file.toString())
  def matches(input: String): Boolean = include.pattern.matcher(input).find() &&
    !exclude.pattern.matcher(input).find()
}

object FilterMatcher {
  val default = FilterMatcher(".*".r, "^$".r)

  implicit val filterMatcherSurface: generic.Surface[FilterMatcher] = generic
    .deriveSurface[FilterMatcher]
  implicit val filterMatcherEncoder: ConfEncoder[FilterMatcher] = generic
    .deriveEncoder[FilterMatcher]
  implicit val filterMatcherDecoder: ConfDecoderEx[FilterMatcher] = generic
    .deriveDecoderEx(default)

  implicit val regexEncoder: ConfEncoder[Regex] = ConfEncoder.StringEncoder
    .contramap[Regex](_.regex)
  implicit val regexDecoder: ConfDecoderEx[Regex] = {
    val seqStringDecoder = implicitly[ConfDecoderEx[Seq[String]]]
    (state, conf) =>
      seqStringDecoder.read(state.map(x => Seq(x.regex)), conf) match {
        case Configured.Ok(x) => Configured.Ok(mkRegexp(x))
        case x: Configured.NotOk => conf match {
            case Conf.Str(s) => Configured.Ok(s.r)
            case _ => x
          }
      }
  }

  def mkRegexp(filters: Seq[String], strict: Boolean = false): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ if strict => filters.mkString("^(", "|", ")$").r
      case _ => filters.mkString("(", "|", ")").r
    }

  def apply(includes: Seq[String], excludes: Seq[String]): FilterMatcher =
    new FilterMatcher(mkRegexp(includes), mkRegexp(excludes))
}
