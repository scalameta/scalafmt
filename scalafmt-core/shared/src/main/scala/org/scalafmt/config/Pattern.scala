package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

case class Pattern(
    includeFilters: Seq[String],
    excludeFilters: Seq[String]
) {
  lazy val matcher: FilterMatcher = new FilterMatcher(
    FilterMatcher.mkRegexp(includeFilters),
    FilterMatcher.mkRegexp(excludeFilters, true)
  )
}

object Pattern {
  implicit lazy val surface: Surface[Pattern] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[Pattern] =
    generic.deriveCodecEx(neverInfix).noTypos
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
