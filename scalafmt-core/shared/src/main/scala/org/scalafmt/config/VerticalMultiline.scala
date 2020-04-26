package org.scalafmt.config

import metaconfig._

/**
  * Configuration related to multi-line formatting.
  */
case class VerticalMultiline(
    atDefnSite: Boolean = false,
    arityThreshold: Int = 100,
    @annotation.DeprecatedName(
      "newlineBeforeImplicitKW",
      "Use newlines.implicitParamListModifierForce=[before] instead",
      "2.5.0"
    )
    newlineBeforeImplicitKW: Boolean = false,
    @annotation.DeprecatedName(
      "newlineAfterImplicitKW",
      "Use newlines.implicitParamListModifierForce=[after] instead",
      "2.5.0"
    )
    newlineAfterImplicitKW: Boolean = false,
    newlineAfterOpenParen: Boolean = false,
    @annotation.DeprecatedName(
      "excludeDanglingParens",
      "Use danglingParentheses.exclude instead",
      "2.5.0"
    )
    excludeDanglingParens: List[DanglingParentheses.Exclude] = List(
      DanglingParentheses.Exclude.`class`,
      DanglingParentheses.Exclude.`trait`
    )
) {
  val reader: ConfDecoder[VerticalMultiline] =
    generic.deriveDecoder(this).noTypos
}

object VerticalMultiline {
  implicit lazy val surface: generic.Surface[VerticalMultiline] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[VerticalMultiline] =
    generic.deriveEncoder[VerticalMultiline]
}
