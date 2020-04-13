package org.scalafmt.config

import metaconfig._

case class Literals(
    long: Case = Case.Upper,
    float: Case = Case.Lower,
    double: Case = Case.Lower,
    hexBody: Case = Case.Lower,
    hexPrefix: Case = Case.Lower,
    scientific: Case = Case.Lower
) {
  implicit val reader: ConfDecoder[Literals] =
    generic.deriveDecoder(this).noTypos
}

object Literals {
  implicit val surface: generic.Surface[Literals] = generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[Literals] = generic.deriveEncoder
}
