package org.scalafmt.config

import Literals.Case
import metaconfig._

case class Literals(
    long: Case = Case.Upper,
    float: Case = Case.Lower,
    double: Case = Case.Lower,
    hexDigits: Case = Case.Lower,
    hexPrefix: Case = Case.Lower,
    binPrefix: Case = Case.Lower,
    scientific: Case = Case.Lower,
)

object Literals {
  implicit val surface: generic.Surface[Literals] = generic.deriveSurface
  implicit val codec: ConfCodecEx[Literals] = generic.deriveCodecEx(Literals())
    .noTypos

  sealed abstract class Case {
    import Case._
    def process(str: String): String = this match {
      case Unchanged => str
      case Lower => str.toLowerCase()
      case Upper => str.toUpperCase()
    }
  }

  object Case {
    implicit val codec: ConfCodecEx[Case] = ReaderUtil
      .oneOf[Case](Upper, Lower, Unchanged)
    case object Upper extends Case
    case object Lower extends Case
    case object Unchanged extends Case
  }
}
