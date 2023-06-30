package org.scalafmt.config

import scala.collection.mutable

import org.scalafmt.util.ValidationOps

import metaconfig._

/** @param oneline
  *   - if fold, try to fold short docstrings into a single line
  *   - if unfold, unfold a single-line docstring into multiple lines
  *   - if keep, preserve the current formatting
  * @param wrap
  *   if yes, allow reformatting/rewrapping the contents of the docstring
  * @param style
  *   - Asterisk: format intermediate lines with an asterisk below the first
  *     asterisk of the first line (aka JavaDoc)
  *   - SpaceAsterisk: format intermediate lines with a space and an asterisk,
  *     both below the two asterisks of the first line
  *   - AsteriskSpace: format intermediate lines with an asterisk and a space,
  *     both below the two asterisks of the first line
  */
case class Docstrings(
    oneline: Docstrings.Oneline = Docstrings.Oneline.keep,
    removeEmpty: Boolean = false,
    wrap: Docstrings.Wrap = Docstrings.Wrap.yes,
    private[config] val wrapMaxColumn: Option[Int] = None,
    forceBlankLineBefore: Option[Boolean] = None,
    blankFirstLine: Docstrings.BlankFirstLine = Docstrings.BlankFirstLine.no,
    style: Docstrings.Style = Docstrings.SpaceAsterisk
) {
  import Docstrings._

  def skipFirstLineIf(wasBlank: Boolean): Boolean = blankFirstLine match {
    case BlankFirstLine.yes => true
    case BlankFirstLine.no => style.skipFirstLine
    case BlankFirstLine.keep => wasBlank || style.skipFirstLine
  }

  def validate(implicit errors: mutable.Buffer[String]): Unit = {
    import ValidationOps._
    addIf(
      blankFirstLine.eq(BlankFirstLine.keep) && style.eq(Docstrings.Asterisk),
      s"docstrings"
    )
  }

}

object Docstrings {

  implicit val surface: generic.Surface[Docstrings] =
    generic.deriveSurface[Docstrings]
  implicit val codec: ConfCodecEx[Docstrings] =
    generic.deriveCodecEx(Docstrings()).noTypos

  sealed abstract class Style {
    def skipFirstLine: Boolean
  }
  case object Preserve extends Style {
    override def skipFirstLine: Boolean = throw new NotImplementedError(
      "skipFirstLine called for docstrings.style=preserve, it's a bug in scalafmt"
    )
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

  implicit val reader: ConfCodecEx[Style] =
    ReaderUtil.oneOfCustom[Style](
      Preserve,
      Asterisk,
      SpaceAsterisk,
      AsteriskSpace
    ) { case Conf.Str("keep") =>
      Configured.Ok(Preserve)
    }

  sealed abstract class Oneline
  object Oneline {
    case object keep extends Oneline
    case object fold extends Oneline
    case object unfold extends Oneline
    implicit val reader: ConfCodecEx[Oneline] =
      ReaderUtil.oneOf[Oneline](keep, fold, unfold)
  }

  sealed abstract class Wrap
  object Wrap {
    case object no extends Wrap
    case object yes extends Wrap
    implicit val codec: ConfCodecEx[Wrap] =
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
    implicit val codec: ConfCodecEx[BlankFirstLine] =
      ReaderUtil.oneOfCustom[BlankFirstLine](yes, no, keep) {
        case Conf.Bool(true) => Configured.Ok(yes)
        case Conf.Bool(false) => Configured.Ok(no)
      }
  }

}
