package org.scalafmt.config

/**
  * ADT representing import selectors settings, specifically pertaining to the
  * handling when multiple names are imported from the same package.
  *
  * When [[org.scalafmt.config.ImportSelectors.noBinPack]] is selected, imports
  * are organized such that each line contains a single name imported from the
  * base package:
  * {{{
  *   // max columns     |
  *   import org.{
  *     Aaaa,
  *     Bbbb,
  *     C,
  *     D,
  *     Eeee
  *   }
  * }}}
  *
  * When [[org.scalafmt.config.ImportSelectors.binPack]] is selected, imports
  * are organized such that each line contains as many names as will fit within
  * the column limit:
  * {{{
  *   // max columns     |
  *   import org.{
  *     Aaaa, Bbbb, C, D,
  *     Eeee
  *   }
  * }}}
  *
  * When [[org.scalafmt.config.ImportSelectors.singleLine]] is selected,
  * imports are organized such that all names for a single package are arranged
  * on a single line:
  * {{{
  *   // max columns     |
  *   import org.{Aaaa, Bbbb, C, D, Eeee}
  * }}}
  *
  */
sealed abstract class ImportSelectors

object ImportSelectors {

  val reader =
    ReaderUtil.oneOf[ImportSelectors](noBinPack, binPack, singleLine)

  case object noBinPack extends ImportSelectors
  case object binPack extends ImportSelectors
  case object singleLine extends ImportSelectors

}
