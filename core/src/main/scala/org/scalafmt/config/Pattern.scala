package org.scalafmt.config

import scala.util.matching.Regex

import metaconfig.ConfigReader
import org.scalafmt.util.RegexOps

@ConfigReader
case class Pattern(
    includeFilters: Seq[String],
    excludeFilters: Seq[String]
) {
  def toMatcher: Matcher =
    new Matcher(RegexOps.mkRegexp(includeFilters),
                RegexOps.mkRegexp(excludeFilters))
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
      "be",
      "synchronized",
      "have"
    )
  )
}
