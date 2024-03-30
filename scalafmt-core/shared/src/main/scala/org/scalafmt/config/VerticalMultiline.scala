package org.scalafmt.config

import metaconfig._

/** Configuration related to multi-line formatting.
  */
case class VerticalMultiline(
    atDefnSite: Boolean = false,
    arityThreshold: Int = 100,
    newlineAfterOpenParen: Boolean = false,
)

object VerticalMultiline {
  implicit lazy val surface: generic.Surface[VerticalMultiline] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[VerticalMultiline] = generic
    .deriveCodecEx(VerticalMultiline()).noTypos
}
