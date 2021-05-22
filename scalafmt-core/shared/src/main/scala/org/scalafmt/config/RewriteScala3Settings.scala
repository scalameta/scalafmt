package org.scalafmt.config

import org.scalafmt.config.RewriteScala3Settings._

import metaconfig._

case class RewriteScala3Settings(
    convertToNewSyntax: Boolean = false,
    removeOptionalBraces: RemoveOptionalBraces = RemoveOptionalBraces.no
) extends Decodable[RewriteScala3Settings] {
  override protected[config] def baseDecoder: ConfDecoder[T] =
    generic.deriveDecoder(this).noTypos
}

object RewriteScala3Settings {
  implicit val surface: generic.Surface[RewriteScala3Settings] =
    generic.deriveSurface
  implicit val encoder: ConfEncoder[RewriteScala3Settings] =
    generic.deriveEncoder[RewriteScala3Settings]

  implicit val preset: PartialFunction[Conf, RewriteScala3Settings] = {
    case Conf.Bool(true) =>
      new RewriteScala3Settings(true, RemoveOptionalBraces.yes)
    case Conf.Bool(false) => new RewriteScala3Settings
  }

  sealed abstract class RemoveOptionalBraces

  object RemoveOptionalBraces {

    implicit val codec: ConfCodec[RemoveOptionalBraces] = ReaderUtil
      .oneOfCustom[RemoveOptionalBraces](yes, no, oldSyntaxToo) {
        case Conf.Bool(true) => Configured.Ok(yes)
        case Conf.Bool(false) => Configured.Ok(no)
      }

    case object no extends RemoveOptionalBraces
    case object yes extends RemoveOptionalBraces
    case object oldSyntaxToo extends RemoveOptionalBraces

  }

}
