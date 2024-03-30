package org.scalafmt.config

import metaconfig._

case class Literals(
    long: Case = Case.Upper,
    float: Case = Case.Lower,
    double: Case = Case.Lower,
    hexDigits: Case = Case.Lower,
    hexPrefix: Case = Case.Lower,
    scientific: Case = Case.Lower
)

object Literals {
  implicit val surface: generic.Surface[Literals] = generic.deriveSurface
  implicit val codec: ConfCodecEx[Literals] = generic.deriveCodecEx(Literals())
    .noTypos
}
