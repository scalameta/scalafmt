package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

case class Pattern(
    includeFilters: Seq[String],
    excludeFilters: Seq[String]
) {
  val reader: ConfDecoder[Pattern] = generic.deriveDecoder(this).noTypos
  lazy val matcher: FilterMatcher = new FilterMatcher(
    FilterMatcher.mkRegexp(includeFilters),
    FilterMatcher.mkRegexp(excludeFilters, true)
  )
}

object Pattern {
  implicit lazy val surface: Surface[Pattern] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[Pattern] =
    generic.deriveEncoder
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
      "ignore",
      "be",
      "of",
      "taggedAs",
      "thrownBy",
      "synchronized",
      "have",
      "when",
      "size",
      "only",
      "noneOf",
      "oneElementOf",
      "noElementsOf",
      "atLeastOneElementOf",
      "atMostOneElementOf",
      "allElementsOf",
      "inOrderElementsOf",
      "theSameElementsAs"
    )
  )
}
