package org.scalafmt.config

import Literals.Case
import metaconfig._

@annotation.SectionRename("float", "floatingPoint.float") // v3.10.3
@annotation.SectionRename("double", "floatingPoint.double") // v3.10.3
@annotation.SectionRename("scientific", "floatingPoint.scientific") // v3.10.3
case class Literals(
    long: Case = Case.Upper,
    hexDigits: Case = Case.Lower,
    hexPrefix: Case = Case.Lower,
    binPrefix: Case = Case.Lower,
    floatingPoint: Literals.FloatingPoint = Literals.FloatingPoint.default,
)

object Literals {
  val default = Literals()

  implicit val surface: generic.Surface[Literals] = generic.deriveSurface
  implicit val codec: ConfCodecEx[Literals] = generic.deriveCodecEx(default)
    .noTypos.detectSectionRenames

  sealed abstract class Case {
    import Case._
    def process(str: String): String = this match {
      case Keep => str
      case Lower => str.toLowerCase()
      case Upper => str.toUpperCase()
    }
    def process(ch: Char): Char = this match {
      case Keep => ch
      case Lower => Character.toLowerCase(ch)
      case Upper => Character.toUpperCase(ch)
    }
  }

  object Case {
    implicit val codec: ConfCodecEx[Case] = ReaderUtil
      .oneOfCustom[Case](Upper, Lower, Keep) { // aliases
        case Conf.Str(str) if str.equalsIgnoreCase("unchanged") =>
          Configured.Ok(Keep)
      }
    case object Upper extends Case
    case object Lower extends Case
    case object Keep extends Case
  }

  case class FloatingPoint(
      float: Case = Case.Lower,
      double: Case = Case.Lower,
      scientific: Case = Case.Lower,
  )

  object FloatingPoint {
    val default = FloatingPoint()

    implicit val surface: generic.Surface[FloatingPoint] = generic
      .deriveSurface[FloatingPoint]
    implicit val codec: ConfCodecEx[FloatingPoint] = generic
      .deriveCodecEx(default).noTypos
  }

}
