package org.scalafmt.config

import org.scalafmt.internal._

import scala.meta.tokens.Token

import metaconfig._

case class RedundantBracesSettings(
    @annotation.ExtraName("methodBodies")
    defnBodies: RedundantBracesSettings.DefnBodies =
      RedundantBracesSettings.DefnBodies.all,
    includeUnitMethods: Boolean = true,
    @annotation.ExtraName("maxLines")
    maxBreaks: Int = 100,
    stringInterpolation: Boolean = false,
    oneStatApply: RedundantBracesSettings.OneStatApply =
      RedundantBracesSettings.OneStatApply.default,
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
    .mapDecoder(
      generic.deriveDecoderEx(default).noTypos
        .withSectionRenames(annotation.SectionRename(
          "parensForOneLineApply",
          "oneStatApply.parensMaxSpan",
          { case Conf.Bool(value) => Conf.Num(if (value) 0 else -1) },
        )),
      "RedundantBraces",
    )

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

  case class OneStatApply(parensMaxSpan: Int = 0, bracesMinSpan: Int = -1) {
    def changeDelim(lt: FT, rt: FT)(implicit ftoks: FormatTokens): AnyRef =
      if (bracesMinSpan < 0 && parensMaxSpan <= 0) null
      else {
        val span = ftoks.offsetDiff(lt, rt)(_.nonWsNonPunct)
        if (parensMaxSpan >= span) Token.LeftParen
        else if (bracesMinSpan >= 0 && bracesMinSpan < span) Token.LeftBrace
        else null
      }
  }

  object OneStatApply {
    val default = OneStatApply()
    implicit val surface: generic.Surface[OneStatApply] = generic.deriveSurface
    implicit val codec: ConfCodecEx[OneStatApply] = generic
      .deriveCodecEx(default).noTypos
  }

}
