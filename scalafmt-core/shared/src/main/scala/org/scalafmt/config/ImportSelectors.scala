package org.scalafmt.config

import metaconfig._

/** ADT representing import selectors settings, specifically pertaining to the
  * handling when multiple names are imported from the same package.
  *
  * When [[org.scalafmt.config.ImportSelectors.unfold]] is selected, imports are
  * organized such that each line contains a single name imported from the base
  * package:
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
  * When [[org.scalafmt.config.ImportSelectors.fold]] is selected, imports are
  * organized such that each line contains as many names as will fit within the
  * column limit:
  * {{{
  *   // max columns     |
  *   import org.{
  *     Aaaa, Bbbb, C, D,
  *     Eeee
  *   }
  * }}}
  *
  * When [[org.scalafmt.config.ImportSelectors.singleLine]] is selected, imports
  * are organized such that all names for a single package are arranged on a
  * single line:
  * {{{
  *   // max columns     |
  *   import org.{Aaaa, Bbbb, C, D, Eeee}
  * }}}
  */
sealed abstract class ImportSelectors

object ImportSelectors {

  implicit val codec: ConfCodecEx[ImportSelectors] = ConfCodecEx
    .oneOfCustom[ImportSelectors](unfold, fold, singleLine, keep) {
      case Conf.Str("binPack") | Conf.Bool(true) => Conf.nameOf(fold)
      case Conf.Str("noBinPack") | Conf.Bool(false) => Conf.nameOf(unfold)
    }

  case object keep extends ImportSelectors
  case object fold extends ImportSelectors
  case object unfold extends ImportSelectors
  case object singleLine extends ImportSelectors

}
