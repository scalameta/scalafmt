package org.scalafmt.config

import metaconfig._

/**
  * Configuration related to multi-line formatting.
  */
case class VerticalMultiline(
    atDefnSite: Boolean = false,
    arityThreshold: Int = 100,
    newlineBeforeImplicitKW: Boolean = false,
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
}

sealed abstract class DanglingExclude

object DanglingExclude {
  case object `class` extends DanglingExclude
  case object `trait` extends DanglingExclude

  implicit val danglingExcludeReader: ConfDecoder[DanglingExclude] =
    ReaderUtil.oneOf[DanglingExclude](`class`, `trait`)
}
