package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

case class Pattern(
    includeFilters: Seq[String],
    excludeFilters: Seq[String]
) {
  val reader: ConfDecoder[Pattern] = generic.deriveDecoder(this).noTypos
  def toMatcher: FilterMatcher =
    new FilterMatcher(
      FilterMatcher.mkRegexp(includeFilters),
      FilterMatcher.mkRegexp(excludeFilters, true))
}

object Pattern {
  implicit val surface: Surface[Pattern] = generic.deriveSurface
  val neverInfix = Pattern(
    Seq("[\\w\\d_]+"),
    Seq(
      "until",
      "to",
      "by",
      "eq",
      "ne",
      "should.*",
      "contain.*",
      "must.*",
      "in",
      "be",
      "taggedAs",
      "thrownBy",
      "synchronized",
      "have",
      "when",
      "size"
    )
  )
}
