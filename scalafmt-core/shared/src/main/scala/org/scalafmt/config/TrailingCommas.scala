package org.scalafmt.config

import metaconfig.Configured.Ok
import metaconfig._

/**
  * ADT representing trailing commas settings
  *
  * When [[org.scalafmt.config.TrailingCommas.always]] is selected, trailing
  * commas are added everywhere a newline is followed by a right parens, brace
  * or bracket.
  *
  * When [[org.scalafmt.config.TrailingCommas.never]] is selected, trailing
  * commas are removed whenever they appear.
  *
  * When [[org.scalafmt.config.TrailingCommas.preserve]] is selected, existing
  * trailing commas will be preserved, and no new ones will be added.
  *
  */
sealed abstract class TrailingCommas

object TrailingCommas {

  implicit val reader: ConfCodec[TrailingCommas] =
    ReaderUtil.oneOf[TrailingCommas](always, never, preserve)

  case object always extends TrailingCommas
  case object never extends TrailingCommas
  case object preserve extends TrailingCommas

}
