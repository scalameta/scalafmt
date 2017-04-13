package org.scalafmt.config

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
@DeriveConfDecoder
case class IndentOperator(
    include: String = ".*",
    exclude: String = "^(&&|\\|\\|)$"
) {
  val includeRegexp = include.r
  val excludeRegexp = exclude.r
}

object IndentOperator {
  val default = IndentOperator()
  implicit val IndentOperatorDecoder: ConfDecoder[IndentOperator] =
    default.reader
  val akka = IndentOperator(ScalafmtConfig.indentOperatorsIncludeAkka,
                            ScalafmtConfig.indentOperatorsExcludeAkka)
}
