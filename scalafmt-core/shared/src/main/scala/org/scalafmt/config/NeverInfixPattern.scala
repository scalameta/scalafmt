package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

case class NeverInfixPattern(
    includeFilters: Seq[String],
    excludeFilters: Seq[String]
) {
  lazy val matcher: FilterMatcher = new FilterMatcher(
    FilterMatcher.mkRegexp(includeFilters),
    FilterMatcher.mkRegexp(excludeFilters, true)
  )

  private[config] def forSbt: Option[NeverInfixPattern] =
    // if the user customized these, we don't touch
    if (excludeFilters ne NeverInfixPattern.default.excludeFilters) None
    else Some(copy(excludeFilters = NeverInfixPattern.sbtExclude))
}

object NeverInfixPattern {
  implicit lazy val surface: Surface[NeverInfixPattern] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[NeverInfixPattern] =
    generic.deriveCodecEx(default).noTypos
  val default = NeverInfixPattern(
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

  private val sbtExclude = Seq(
    "cross"
  ) ++ default.excludeFilters
}
