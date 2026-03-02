package org.scalafmt.config

import org.scalafmt.config.RewriteScala3Settings._

import metaconfig._

@annotation.SectionRename("countEndMarkerLines", "endMarker.spanHas") // renamed in v3.10.3
@annotation.SectionRename("removeEndMarkerMaxLines", "endMarker.removeMaxSpan") // renamed in v3.10.3
@annotation.SectionRename("insertEndMarkerMinLines", "endMarker.insertMinSpan") // renamed in v3.10.3
case class RewriteScala3Settings(
    convertToNewSyntax: Boolean = false,
    newSyntax: ConvertToNewSyntax = ConvertToNewSyntax.default,
    removeOptionalBraces: RemoveOptionalBraces = RemoveOptionalBraces.no,
    endMarker: EndMarker = EndMarker.default,
)

object RewriteScala3Settings {
  implicit val surface: generic.Surface[RewriteScala3Settings] =
    generic.deriveSurface
  implicit val encoder: ConfEncoder[RewriteScala3Settings] = generic
    .deriveEncoder[RewriteScala3Settings]

  val default = new RewriteScala3Settings

  implicit val decodec: ConfDecoderEx[RewriteScala3Settings] = Presets
    .mapDecoder(
      generic.deriveDecoderEx(default).noTypos.detectSectionRenames,
      "rewrite.scala3",
    ) {
      case Conf.Bool(true) => new RewriteScala3Settings(
          convertToNewSyntax = true,
          removeOptionalBraces = RemoveOptionalBraces.yes,
        )
      case Conf.Bool(false) => default
    }

  case class RemoveOptionalBraces(
      enabled: Boolean = true,
      private[config] val removeBracesMaxSpan: Int = 0,
      private[config] val removeBracesMaxBlankGaps: Int = -1,
      fewerBracesMinSpan: Int = 2,
      fewerBracesMaxSpan: Int = 0,
      fewerBracesParensToo: Boolean = false,
      oldSyntaxToo: Boolean = false,
  ) {
    def isRemoveEnabled: Boolean = getRemoveBracesMaxSpan >= 0 ||
      getRemoveBracesMaxBlankGaps >= 0

    def getRemoveBracesMaxSpan: Int = removeBracesMaxSpan

    def getRemoveBracesMaxBlankGaps: Int = removeBracesMaxBlankGaps
  }

  object RemoveOptionalBraces {

    val yes = RemoveOptionalBraces()
    val no = RemoveOptionalBraces(enabled = false)

    implicit val surface: generic.Surface[RemoveOptionalBraces] =
      generic.deriveSurface

    implicit val encoder: ConfEncoder[RemoveOptionalBraces] = generic
      .deriveEncoder[RemoveOptionalBraces]

    implicit final val decoder: ConfDecoderEx[RemoveOptionalBraces] = generic
      .deriveDecoderEx[RemoveOptionalBraces](no).contramap {
        case Conf.Bool(true) | Conf.Str("yes") => Conf
            .Obj("enabled" -> Conf(true))
        case Conf.Bool(false) | Conf.Str("no") => Conf
            .Obj("enabled" -> Conf(false))
        case Conf.Str("oldSyntaxToo") => Conf
            .Obj("enabled" -> Conf(true), "oldSyntaxToo" -> Conf(true))
        case conf => conf
      }
  }

  case class EndMarker(
      spanIs: EndMarker.SpanIs = EndMarker.SpanIs.lines,
      spanHas: EndMarker.SpanHas = EndMarker.SpanHas.all,
      removeMaxSpan: Int = 0,
      insertMinSpan: Int = 0,
  )

  object EndMarker {

    val default = new EndMarker
    implicit val surface: generic.Surface[EndMarker] = generic.deriveSurface
    implicit val codec: ConfCodecEx[EndMarker] = generic.deriveCodecEx(default)
      .noTypos

    sealed abstract class SpanIs
    object SpanIs {
      implicit val codec: ConfCodecEx[SpanIs] = ConfCodecEx
        .oneOf(lines, blankGaps)
      case object lines extends SpanIs
      case object blankGaps extends SpanIs
    }

    sealed abstract class SpanHas
    object SpanHas {
      implicit val codec: ConfCodecEx[SpanHas] = ConfCodecEx
        .oneOf(all, lastBlockOnly)
      case object all extends SpanHas
      case object lastBlockOnly extends SpanHas
    }

  }

  case class ConvertToNewSyntax(
      // https://dotty.epfl.ch/docs/reference/other-new-features/control-syntax.html
      control: Boolean = true,
      // https://dotty.epfl.ch/docs/reference/changed-features/vararg-splices.html
      // https://dotty.epfl.ch/docs/reference/changed-features/imports.html
      // https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html
      deprecated: Boolean = true,
  )

  private object ConvertToNewSyntax {

    val default = new ConvertToNewSyntax

    implicit val surface: generic.Surface[ConvertToNewSyntax] =
      generic.deriveSurface
    implicit val codec: ConfCodecEx[ConvertToNewSyntax] = generic
      .deriveCodecEx(default).noTypos

  }

}
