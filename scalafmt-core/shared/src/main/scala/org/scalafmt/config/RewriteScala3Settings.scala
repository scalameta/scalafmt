package org.scalafmt.config

import org.scalafmt.config.RewriteScala3Settings._

import metaconfig._

case class RewriteScala3Settings(
    convertToNewSyntax: Boolean = false,
    removeOptionalBraces: RemoveOptionalBraces = RemoveOptionalBraces.no,
    removeEndMarkerMaxLines: Int = 0,
    insertEndMarkerMinLines: Int = 0
)

object RewriteScala3Settings {
  implicit val surface: generic.Surface[RewriteScala3Settings] =
    generic.deriveSurface
  implicit val encoder: ConfEncoder[RewriteScala3Settings] =
    generic.deriveEncoder[RewriteScala3Settings]

  private val default = new RewriteScala3Settings

  implicit val decodec: ConfDecoderEx[RewriteScala3Settings] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "rewrite.scala3") {
      case Conf.Bool(true) =>
        new RewriteScala3Settings(true, RemoveOptionalBraces.yes)
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

}
