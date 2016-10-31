package org.scalafmt.config

/**
  * ADT representing import selectors settings.
  * TODO(mtomko) provide a brief description of each setting, with code examples of what each setting does
  */
sealed abstract class ImportSelectors

object ImportSelectors {

  val reader =
    ReaderUtil.oneOf[ImportSelectors](noBinPack, binPack, singleLine)
  case object noBinPack extends ImportSelectors
  case object binPack extends ImportSelectors
  case object singleLine extends ImportSelectors

}
