package org.scalafmt.config

import metaconfig._

case class RedundantParensSettings(
    infixSide: Option[RedundantParensSettings.InfixSide] = None
)

object RedundantParensSettings {
  val default = RedundantParensSettings()
  implicit lazy val surface: generic.Surface[RedundantParensSettings] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[RedundantParensSettings] = generic
    .deriveCodecEx(default).noTypos

  sealed abstract class InfixSide
  object InfixSide {
    implicit val codec: ConfCodecEx[InfixSide] = ReaderUtil
      .oneOf[InfixSide](all, many, some)
    case object all extends InfixSide
    case object many extends InfixSide
    case object some extends InfixSide
  }

}
