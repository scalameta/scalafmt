package org.scalafmt.config

import metaconfig._
import metaconfig.annotation.ExtraName

case class RedundantBracesSettings(
    @ExtraName("methodBodies")
    defnBodies: RedundantBracesSettings.DefnBodies =
      RedundantBracesSettings.DefnBodies.all,
    includeUnitMethods: Boolean = true,
    @ExtraName("maxLines")
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

  implicit lazy val surface: generic.Surface[RedundantBracesSettings] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[RedundantBracesSettings] = generic
    .deriveCodecEx(default).noTypos

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
