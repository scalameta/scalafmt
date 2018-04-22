package org.scalafmt.config

import metaconfig.Configured.Ok
import metaconfig._

/**
  *
  * @param include
  *   Regexp for which infix operators should
  *   indent by 2 spaces. For example, .*=
  *   produces this output
  *
  *   a &&
  *   b
  *
  *   a +=
  *     b
  * @param exclude
  *   Regexp for which infix operators should
  *   not indent by 2 spaces. For example, when
  *   [[include]] is .* and
  *   [[exclude]] is &&
  *
  *   a &&
  *   b
  *
  *   a ||
  *     b
  *
  *   a +=
  *     b
  */
case class IndentOperator(
    include: String = ".*",
    exclude: String = "^(&&|\\|\\|)$"
) {
  val reader: ConfDecoder[IndentOperator] = generic.deriveDecoder(this)

  val includeRegexp = include.r
  val excludeRegexp = exclude.r
}

object IndentOperator {
  implicit val surface: generic.Surface[IndentOperator] =
    generic.deriveSurface
  val default = IndentOperator()
  val akka = IndentOperator(
    ScalafmtConfig.indentOperatorsIncludeAkka,
    ScalafmtConfig.indentOperatorsExcludeAkka)
  implicit val IndentOperatorDecoder: ConfDecoder[IndentOperator] =
    ConfDecoder.instance[IndentOperator] {
      case Conf.Str("spray" | "akka" | "akka-http") =>
        Ok(IndentOperator.akka)
      case Conf.Str("default") =>
        Ok(IndentOperator.default)
      case els =>
        default.reader.read(els)
    }
}
