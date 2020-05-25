package org.scalafmt.config

import metaconfig._

/**
  * @param oneline
  *        - if fold, try to fold short docstrings into a single line
  *        - if unfold, unfold a single-line docstring into multiple lines
  * @param style
  *        - preserve: keep existing formatting
  *        - Asterisk: format intermediate lines with an asterisk below the
  *          first asterisk of the first line (aka JavaDoc)
  *        - SpaceAsterisk: format intermediate lines with a space and
  *          an asterisk, both below the two asterisks of the first line
  */
case class Docstrings(
    oneline: Docstrings.Oneline = Docstrings.Oneline.keep,
    style: Option[Docstrings.Style] = Some(Docstrings.SpaceAsterisk)
) {
  import Docstrings._

  def isSpaceAsterisk: Boolean = style.contains(SpaceAsterisk)

  implicit lazy val decoder: ConfDecoder[Docstrings] = {
    val genericDecoder = generic.deriveDecoder(this).noTypos
    new ConfDecoder[Docstrings] {
      override def read(conf: Conf): Configured[Docstrings] =
        conf match {
          case Conf.Str("preserve") =>
            Configured.ok(copy(style = None))
          case Conf.Str("ScalaDoc") =>
            Configured.ok(copy(style = Some(SpaceAsterisk)))
          case Conf.Str("JavaDoc") =>
            Configured.ok(copy(style = Some(Asterisk)))
          case _: Conf.Str =>
            reader.read(conf).map(x => copy(style = Some(x)))
          case _ => genericDecoder.read(conf)
        }
    }
  }

}

object Docstrings {

  implicit val surface: generic.Surface[Docstrings] =
    generic.deriveSurface[Docstrings]
  implicit val encoder = generic.deriveEncoder[Docstrings]

  sealed abstract class Style
  case object Asterisk extends Style
  case object SpaceAsterisk extends Style

  implicit val reader: ConfCodec[Style] =
    ReaderUtil.oneOf[Style](Asterisk, SpaceAsterisk)

  sealed abstract class Oneline
  object Oneline {
    case object keep extends Oneline
    case object fold extends Oneline
    case object unfold extends Oneline
    implicit val reader: ConfCodec[Oneline] =
      ReaderUtil.oneOf[Oneline](keep, fold, unfold)
  }

}
