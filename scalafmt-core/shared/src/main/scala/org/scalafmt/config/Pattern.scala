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

  private[config] def forSbt: Option[Pattern] =
    // if the user customized these, we don't touch
    if (excludeFilters ne Pattern.neverInfix.excludeFilters) None
    else Some(copy(excludeFilters = Pattern.sbtExclude))
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

  private val sbtExclude = Seq(
    "cross"
  ) ++ neverInfix.excludeFilters
}
