package org.scalafmt.config

import metaconfig._

/**
  * @param include
  *   Regexp for which infix operators should
  *   indent by 2 spaces. For example, .*=
  *   produces this output
  * {{{
  *   a &&
  *   b
  *
  *   a +=
  *     b
  * }}}
  * @param exclude
  *   Regexp for which infix operators should
  *   not indent by 2 spaces. For example, when
  *   [[include]] is .* and
  *   [[exclude]] is &&
  * {{{
  *   a &&
  *   b
  *
  *   a ||
  *     b
  *
  *   a +=
  *     b
  * }}}
  * @param topLevelOnly
  *   If true, allows no indentation on infix operators in top-level functions only.
  *   For example,
  * {{{
  *   // top-level, flag doesn't apply
  *   a &&
  *   b
  *   // true
  *   function(
  *     a &&
  *       b
  *   )
  *   // false
  *   function(
  *     a &&
  *     b
  *   )
  * }}}
  * @see For context:
  * [[https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md#long-expressions-with-binary-operators]]
  */
case class IndentOperator(
    topLevelOnly: Boolean = true,
    include: String = ".*",
    exclude: String = "^(&&|\\|\\|)$"
) extends Decodable[IndentOperator] {
  override protected[config] def baseDecoder =
    generic.deriveDecoder(this).noTypos

  private val includeRegexp = include.r.pattern
  private val excludeRegexp = exclude.r.pattern

  def noindent(op: String): Boolean =
    excludeRegexp.matcher(op).find() || !includeRegexp.matcher(op).find()
}

object IndentOperator {
  private val default = IndentOperator()
  private val akka = IndentOperator(include = "^.*=$", exclude = "^$")

  implicit lazy val surface: generic.Surface[IndentOperator] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[IndentOperator] =
    generic.deriveEncoder

  implicit val preset: PartialFunction[Conf, IndentOperator] = {
    case Conf.Str("spray" | "akka" | "akka-http") => IndentOperator.akka
    case Conf.Str("default") => IndentOperator.default
  }
}
