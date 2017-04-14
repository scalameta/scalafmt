package org.scalafmt.config

import metaconfig._

@DeriveConfDecoder
case class Pattern(
    includeFilters: Seq[String],
    excludeFilters: Seq[String]
) {
  def toMatcher: FilterMatcher =
    new FilterMatcher(FilterMatcher.mkRegexp(includeFilters),
                      FilterMatcher.mkRegexp(excludeFilters, true))
}

object Pattern {
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
      "when"
    )
  )
}
