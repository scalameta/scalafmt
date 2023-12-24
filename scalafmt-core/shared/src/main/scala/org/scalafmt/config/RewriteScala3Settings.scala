package org.scalafmt.config

import org.scalafmt.config.RewriteScala3Settings._

import metaconfig._

case class RewriteScala3Settings(
    convertToNewSyntax: Boolean = false,
    newSyntax: ConvertToNewSyntax = ConvertToNewSyntax.default,
    removeOptionalBraces: RemoveOptionalBraces = RemoveOptionalBraces.no,
    countEndMarkerLines: RewriteScala3Settings.EndMarkerLines =
      RewriteScala3Settings.EndMarkerLines.all,
    removeEndMarkerMaxLines: Int = 0,
    insertEndMarkerMinLines: Int = 0
)

object RewriteScala3Settings {
  implicit val surface: generic.Surface[RewriteScala3Settings] =
    generic.deriveSurface
  implicit val encoder: ConfEncoder[RewriteScala3Settings] =
    generic.deriveEncoder[RewriteScala3Settings]

  val default = new RewriteScala3Settings

  implicit val decodec: ConfDecoderEx[RewriteScala3Settings] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "rewrite.scala3") {
      case Conf.Bool(true) =>
        new RewriteScala3Settings(
          convertToNewSyntax = true,
          removeOptionalBraces = RemoveOptionalBraces.yes
        )
      case Conf.Bool(false) => default
    }

  sealed abstract class RemoveOptionalBraces

  object RemoveOptionalBraces {

    implicit val codec: ConfCodecEx[RemoveOptionalBraces] = ReaderUtil
      .oneOfCustom[RemoveOptionalBraces](yes, no, oldSyntaxToo) {
        case Conf.Bool(true) => Configured.Ok(yes)
        case Conf.Bool(false) => Configured.Ok(no)
      }

    case object no extends RemoveOptionalBraces
    case object yes extends RemoveOptionalBraces
    case object oldSyntaxToo extends RemoveOptionalBraces

  }

  sealed abstract class EndMarkerLines

  object EndMarkerLines {

    implicit val codec: ConfCodecEx[EndMarkerLines] =
      ReaderUtil.oneOf[EndMarkerLines](all, lastBlockOnly)

    case object all extends EndMarkerLines
    case object lastBlockOnly extends EndMarkerLines

  }

  case class ConvertToNewSyntax(
      // https://dotty.epfl.ch/docs/reference/other-new-features/control-syntax.html
      control: Boolean = true,
      // https://dotty.epfl.ch/docs/reference/changed-features/vararg-splices.html
      // https://dotty.epfl.ch/docs/reference/changed-features/imports.html
      // https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html
      deprecated: Boolean = true
  )

  private object ConvertToNewSyntax {

    val default = new ConvertToNewSyntax

    implicit val surface: generic.Surface[ConvertToNewSyntax] =
      generic.deriveSurface
    implicit val codec: ConfCodecEx[ConvertToNewSyntax] =
      generic.deriveCodecEx(default).noTypos

  }

}
