package org.scalafmt.config

import metaconfig._

case class XmlLiterals(
    assumeFormatted: Boolean = false
) {
  implicit val decoder: ConfDecoder[XmlLiterals] =
    generic.deriveDecoder(this).noTypos
}

object XmlLiterals {
  implicit val surface: generic.Surface[XmlLiterals] = generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[XmlLiterals] = generic.deriveEncoder
}
