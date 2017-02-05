package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class Pattern(
    includeFilters: Seq[String],
    excludeFilters: Seq[String]
) {
  def toMatcher: FilterMatcher =
    new FilterMatcher(FilterMatcher.mkRegexp(includeFilters),
                      FilterMatcher.mkRegexp(excludeFilters))
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
