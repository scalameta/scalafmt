package org.scalafmt.config

import metaconfig._

case class RedundantBracesSettings(
    @annotation.ExtraName("methodBodies")
    defnBodies: RedundantBracesSettings.DefnBodies =
      RedundantBracesSettings.DefnBodies.all,
    includeUnitMethods: Boolean = true,
    @annotation.ExtraName("maxLines")
    maxBreaks: Int = 100,
    stringInterpolation: Boolean = false,
    parensForOneLineApply: Boolean = true,
    generalExpressions: Boolean = true,
    ifElseExpressions: Boolean = false,
)

object RedundantBracesSettings {

  val default = RedundantBracesSettings()
  private[scalafmt] val all =
    RedundantBracesSettings(stringInterpolation = true, ifElseExpressions = true)

  private implicit val preset
      : PartialFunction[Conf, RedundantBracesSettings] = {
    case Conf.Str("default") => default
    case Conf.Str("all") => all
  }

  implicit lazy val surface: generic.Surface[RedundantBracesSettings] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[RedundantBracesSettings] =
    generic.deriveEncoder
  implicit lazy val decoder: ConfDecoderEx[RedundantBracesSettings] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "RedundantBraces")

  sealed abstract class DefnBodies

  object DefnBodies {
    implicit val codec: ConfCodecEx[DefnBodies] = ReaderUtil
      .oneOfCustom[DefnBodies](all, none, noParams) {
        case Conf.Bool(true) => Configured.Ok(all)
        case Conf.Bool(false) => Configured.Ok(none)
      }

    case object all extends DefnBodies
    case object none extends DefnBodies
    case object noParams extends DefnBodies
  }

}
