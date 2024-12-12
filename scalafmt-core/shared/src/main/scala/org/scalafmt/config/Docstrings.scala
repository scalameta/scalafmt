package org.scalafmt.config

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
  * @param forceBlankLineBefore
  *   If true, always insert a blank line before docstrings, If false, preserves
  *   blank line only if one exists before. Example:
  *   {{{
  *     // before
  *     object Foo {
  *       /** Docstring */
  *       def foo = 2
  *     }
  *     // after, if forceBlankLineBefore=false
  *     object Foo {
  *       /** Docstring */
  *       def foo = 2
  *     }
  *     // after, if forceBlankLineBefore=true
  *     object Foo {
  *
  *       /** Docstring */
  *       def foo = 2
  *     }
  *   }}}
  */
case class Docstrings(
    oneline: Docstrings.Oneline = Docstrings.Oneline.keep,
    removeEmpty: Boolean = false,
    wrap: Docstrings.Wrap = Docstrings.Wrap.unfold,
    private[config] val wrapMaxColumn: Option[Int] = None,
    forceBlankLineBefore: Boolean = true,
    blankFirstLine: Option[Docstrings.BlankFirstLine] = None,
    style: Docstrings.Style = Docstrings.SpaceAsterisk,
) {
  import Docstrings._

  def withoutRewrites: Docstrings =
    copy(removeEmpty = false, wrap = Wrap.keep, style = Preserve)

  def skipFirstLineIf(wasBlank: Boolean): Boolean = style
    .skipFirstLine(blankFirstLine).exists {
      case BlankFirstLine.unfold => true
      case BlankFirstLine.fold => false
      case BlankFirstLine.keep => wasBlank
    }

}

object Docstrings {

  implicit val surface: generic.Surface[Docstrings] = generic
    .deriveSurface[Docstrings]
  implicit val codec: ConfCodecEx[Docstrings] = generic.deriveCodecEx(Docstrings())
    .noTypos

  sealed abstract class Style {
    def skipFirstLine(v: Option[BlankFirstLine]): Option[BlankFirstLine]
  }
  case object Preserve extends Style {
    def skipFirstLine(v: Option[BlankFirstLine]): Option[BlankFirstLine] =
      Some(BlankFirstLine.keep)
  }
  case object Asterisk extends Style {
    def skipFirstLine(v: Option[BlankFirstLine]): Option[BlankFirstLine] =
      v match {
        case v @ Some(BlankFirstLine.fold) => v
        case _ => Some(BlankFirstLine.unfold)
      }
  }
  case object SpaceAsterisk extends Style {
    def skipFirstLine(v: Option[BlankFirstLine]): Option[BlankFirstLine] = v
  }
  case object AsteriskSpace extends Style {
    def skipFirstLine(v: Option[BlankFirstLine]): Option[BlankFirstLine] = v
  }

  implicit val reader: ConfCodecEx[Style] = ReaderUtil
    .oneOfCustom[Style](Preserve, Asterisk, SpaceAsterisk, AsteriskSpace) {
      case Conf.Str("keep") => Configured.Ok(Preserve)
    }

  sealed abstract class Oneline
  object Oneline {
    case object keep extends Oneline
    case object fold extends Oneline
    case object unfold extends Oneline
    implicit val reader: ConfCodecEx[Oneline] = ReaderUtil
      .oneOf[Oneline](keep, fold, unfold)
  }

  sealed abstract class Wrap
  object Wrap {
    case object keep extends Wrap
    case object fold extends Wrap
    case object unfold extends Wrap
    implicit val codec: ConfCodecEx[Wrap] = ReaderUtil
      .oneOfCustom[Wrap](keep, fold, unfold) {
        case Conf.Str("no") => Configured.Ok(keep)
        case Conf.Bool(false) => Configured.Ok(keep)
        case Conf.Str("yes") => Configured.Ok(unfold)
        case Conf.Bool(true) => Configured.Ok(unfold)
      }
  }

  sealed abstract class BlankFirstLine
  object BlankFirstLine {
    case object unfold extends BlankFirstLine
    case object fold extends BlankFirstLine
    case object keep extends BlankFirstLine
    implicit val codec: ConfCodecEx[BlankFirstLine] = ReaderUtil
      .oneOfCustom[BlankFirstLine](unfold, fold, keep) {
        case Conf.Str("no") => Configured.Ok(fold)
        case Conf.Bool(false) => Configured.Ok(fold)
        case Conf.Str("yes") => Configured.Ok(unfold)
        case Conf.Bool(true) => Configured.Ok(unfold)
      }
  }

}
