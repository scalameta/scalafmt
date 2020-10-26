package org.scalafmt.config

import metaconfig._
import org.scalafmt.config.SpacesInInterpolatedStringCurlyBraces._
import scala.meta.Tree

sealed abstract class SpacesInInterpolatedStringCurlyBraces {
  def areNeeded(inner: Tree, outer: Tree) =
    this == Always ||
      this == IfComplex && (
        inner.children.nonEmpty || !inner.parent.contains(outer)
      )
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

  case object Always extends SpacesInInterpolatedStringCurlyBraces
  case object IfComplex extends SpacesInInterpolatedStringCurlyBraces
  case object Never extends SpacesInInterpolatedStringCurlyBraces
}
