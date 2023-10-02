package org.scalafmt.config

import metaconfig._

case class TrailingCommas(
    allowFolding: Boolean = true,
    style: TrailingCommas.Style = TrailingCommas.never
) {
  def withoutRewrites: TrailingCommas = copy(style = TrailingCommas.keep)
}

object TrailingCommas {

  implicit lazy val surface: generic.Surface[TrailingCommas] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[TrailingCommas] =
    generic.deriveCodecEx(TrailingCommas())

  /** ADT representing trailing commas settings
    *
    * When [[org.scalafmt.config.TrailingCommas.always]] is selected, trailing
    * commas are added everywhere a newline is followed by a right parens, brace
    * or bracket.
    *
    * When [[org.scalafmt.config.TrailingCommas.multiple]] is selected, trailing
    * commas are only added if there's more than one non-repeated argument.
    *
    * When [[org.scalafmt.config.TrailingCommas.never]] is selected, trailing
    * commas are removed whenever they appear.
    *
    * When [[org.scalafmt.config.TrailingCommas.keep]] is selected, existing
    * trailing commas will be preserved, and no new ones will be added.
    */
  sealed abstract class Style

  object Style {
    implicit val codec: ConfCodecEx[Style] =
      ReaderUtil.oneOfCustom[Style](always, never, keep, multiple) {
        case Conf.Str(str) if str.equalsIgnoreCase("preserve") =>
          Configured.Ok(keep)
      }
  }

  case object always extends Style
  case object never extends Style
  case object keep extends Style
  case object multiple extends Style

}
