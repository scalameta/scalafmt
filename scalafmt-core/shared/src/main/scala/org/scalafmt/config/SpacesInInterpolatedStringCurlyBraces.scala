package org.scalafmt.config

import metaconfig._
import org.scalafmt.config.SpacesInInterpolatedStringCurlyBraces._

sealed abstract class SpacesInInterpolatedStringCurlyBraces
    extends Decodable[SpacesInInterpolatedStringCurlyBraces] {

  override protected[config] def baseDecoder =
    SpacesInInterpolatedStringCurlyBraces.decoder

  def isIfComplex = this == IfComplex
  def isAlways = this == Always
}

object SpacesInInterpolatedStringCurlyBraces {

  val codec: ConfCodec[SpacesInInterpolatedStringCurlyBraces] =
    ReaderUtil.oneOf[SpacesInInterpolatedStringCurlyBraces](
      Always,
      IfComplex,
      Never
    )

  implicit val encoder: ConfEncoder[SpacesInInterpolatedStringCurlyBraces] =
    codec
  implicit val decoder: ConfDecoder[SpacesInInterpolatedStringCurlyBraces] =
    codec

  implicit val preset
      : PartialFunction[Conf, SpacesInInterpolatedStringCurlyBraces] = {
    case Conf.Bool(true) => Always
    case Conf.Bool(false) => Never
  }

  case object Always extends SpacesInInterpolatedStringCurlyBraces
  case object IfComplex extends SpacesInInterpolatedStringCurlyBraces
  case object Never extends SpacesInInterpolatedStringCurlyBraces
}
