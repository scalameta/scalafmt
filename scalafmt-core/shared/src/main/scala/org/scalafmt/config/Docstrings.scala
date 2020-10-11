package org.scalafmt.config

import scala.collection.mutable

import org.scalafmt.util.ValidationOps

import metaconfig._

/**
  * @param oneline
  *        - if fold, try to fold short docstrings into a single line
  *        - if unfold, unfold a single-line docstring into multiple lines
  *        - if keep, preserve the current formatting
  * @param wrap
  *   if yes, allow reformatting/rewrapping the contents of the docstring
  * @param style
  *        - Asterisk: format intermediate lines with an asterisk below the
  *          first asterisk of the first line (aka JavaDoc)
  *        - SpaceAsterisk: format intermediate lines with a space and
  *          an asterisk, both below the two asterisks of the first line
  *        - AsteriskSpace: format intermediate lines with an asterisk
  *          and a space, both below the two asterisks of the first line
  */
case class Docstrings(
    oneline: Docstrings.Oneline = Docstrings.Oneline.keep,
    wrap: Docstrings.Wrap = Docstrings.Wrap.no,
    blankFirstLine: Docstrings.BlankFirstLine = Docstrings.BlankFirstLine.no,
    style: Option[Docstrings.Style] = Some(Docstrings.SpaceAsterisk)
) {
  import Docstrings._

  @inline
  def skipFirstLine: Boolean = style.exists(_.skipFirstLine)
  def isSpaceAsterisk: Boolean = style.contains(SpaceAsterisk)
  def isAsteriskSpace: Boolean = style.contains(AsteriskSpace)

  def validate(implicit errors: mutable.Buffer[String]): Unit = {
    import ValidationOps._
    addIf(
      blankFirstLine.ne(BlankFirstLine.no) && wrap.eq(Docstrings.Wrap.yes),
      s"docstrings"
    )
    addIf(
      blankFirstLine.eq(BlankFirstLine.keep) && style.eq(Docstrings.Asterisk),
      s"docstrings"
    )
  }

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

  sealed abstract class Style {
    def skipFirstLine: Boolean
  }
  case object Asterisk extends Style {
    override def skipFirstLine: Boolean = true
  }
  case object SpaceAsterisk extends Style {
    override def skipFirstLine: Boolean = false
  }
  case object AsteriskSpace extends Style {
    override def skipFirstLine: Boolean = false
  }

  implicit val reader: ConfCodec[Style] =
    ReaderUtil.oneOf[Style](Asterisk, SpaceAsterisk, AsteriskSpace)

  sealed abstract class Oneline
  object Oneline {
    case object keep extends Oneline
    case object fold extends Oneline
    case object unfold extends Oneline
    implicit val reader: ConfCodec[Oneline] =
      ReaderUtil.oneOf[Oneline](keep, fold, unfold)
  }

  sealed abstract class Wrap
  object Wrap {
    case object no extends Wrap
    case object yes extends Wrap
    implicit val codec: ConfCodec[Wrap] =
      ReaderUtil.oneOfCustom[Wrap](no, yes) {
        case Conf.Bool(true) => Configured.Ok(yes)
        case Conf.Bool(false) => Configured.Ok(no)
      }
  }

  sealed abstract class BlankFirstLine
  object BlankFirstLine {
    case object yes extends BlankFirstLine
    case object no extends BlankFirstLine
    case object keep extends BlankFirstLine
    implicit val codec: ConfCodec[BlankFirstLine] =
      ReaderUtil.oneOfCustom[BlankFirstLine](yes, no, keep) {
        case Conf.Bool(true) => Configured.Ok(yes)
        case Conf.Bool(false) => Configured.Ok(no)
      }
  }

}
