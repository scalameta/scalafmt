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
      "Use newlines.beforeImplicitParamListModifier instead",
      "2.5.0"
    )
    newlineBeforeImplicitKW: Boolean = false,
    @annotation.DeprecatedName(
      "newlineAfterImplicitKW",
      "Use newlines.afterImplicitParamListModifier instead",
      "2.5.0"
    )
    newlineAfterImplicitKW: Boolean = false,
    newlineAfterOpenParen: Boolean = false,
    excludeDanglingParens: List[DanglingExclude] = List(
      DanglingExclude.`class`,
      DanglingExclude.`trait`
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

sealed abstract class DanglingExclude

object DanglingExclude {
  case object `class` extends DanglingExclude
  case object `trait` extends DanglingExclude
  case object `def` extends DanglingExclude

  implicit val danglingExcludeReader: ConfCodec[DanglingExclude] =
    ReaderUtil.oneOf[DanglingExclude](`class`, `trait`, `def`)
}
