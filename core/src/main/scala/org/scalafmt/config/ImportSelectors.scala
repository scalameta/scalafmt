package org.scalafmt.config

import metaconfig.Reader

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

  // This reader is backwards compatible with the old import selector
  // configuration, which used the boolean flag binPackImportSelectors to
  // decide between (what are now) the `binPack` and `noBinPack` strategies.
  // It is defined here to keep the `Reader.instance[T} {}` lambda in a
  // separate file from the @ConfigReader macro annotation; this is due to
  // limitations in the current version of scalameta/paradise, but these will
  // likely be fixed in the future, at which point this reader could be moved
  // to ScalafmtConfig
  val backwardsCompatibleReader = Reader.instance[ImportSelectors] {
    case true => Right(ImportSelectors.binPack)
    case false => Right(ImportSelectors.noBinPack)
    case els => reader.read(els)
  }

  case object noBinPack extends ImportSelectors
  case object binPack extends ImportSelectors
  case object singleLine extends ImportSelectors

}
