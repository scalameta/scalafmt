package org.scalafmt.config

import metaconfig._

case class RedundantParensSettings(
    infixSide: Option[RedundantParensSettings.InfixSide] = None,
)

object RedundantParensSettings {
  val default = RedundantParensSettings()
  private[scalafmt] val all =
    RedundantParensSettings(infixSide = Some(InfixSide.all))

  private implicit val preset
      : PartialFunction[Conf, RedundantParensSettings] = {
    case Conf.Str("default") => default
    case Conf.Str("all") => all
  }

  implicit lazy val surface: generic.Surface[RedundantParensSettings] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[RedundantParensSettings] =
    generic.deriveEncoder
  implicit lazy val decoder: ConfDecoderEx[RedundantParensSettings] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "RedundantParens")

  sealed abstract class InfixSide
  object InfixSide {
    implicit val codec: ConfCodecEx[InfixSide] = ReaderUtil
      .oneOf[InfixSide](all, many, some)
    case object all extends InfixSide
    case object many extends InfixSide
    case object some extends InfixSide
  }

}
