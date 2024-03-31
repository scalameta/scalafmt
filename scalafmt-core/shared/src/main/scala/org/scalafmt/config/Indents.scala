package org.scalafmt.config

import metaconfig._

/** @param main
  *   the primary indentation used in the code
  * @param significant
  *   the indentation used when optional braces are omitted
  * @param defnSite
  *   indentation around class/def
  * @param ctorSite
  *   indentation around class constructor parameters
  * @param caseSite
  *   indentation for case values before arrow
  * @param callSite
  *   indentation around function calls, etc.
  * @param extendSite
  *   indentation before `extends`
  * @param withSiteRelativeToExtends
  *   additional indentation before `with`
  * @param commaSiteRelativeToExtends
  *   additional indentation before in the line after extends with a comma
  */
case class Indents(
    main: Int = 2,
    private[config] val significant: Option[Int] = None,
    callSite: Int = 2,
    ctrlSite: Option[Int] = None,
    defnSite: Int = 4,
    caseSite: Int = 4,
    matchSite: Option[Int] = None,
    private[config] val ctorSite: Option[Int] = None,
    extraBeforeOpenParenDefnSite: Int = 0,
    relativeToLhsLastLine: Seq[Indents.RelativeToLhs] = Nil,
    private[config] val fewerBraces: Indents.FewerBraces =
      Indents.FewerBraces.never,
    @annotation.ExtraName("deriveSite")
    extendSite: Int = 4,
    withSiteRelativeToExtends: Int = 0,
    commaSiteRelativeToExtends: Int = 2,
) {
  lazy val getSignificant = significant.getOrElse(main)

  def getDefnSite(tree: meta.Tree): Int = (tree match {
    case _: meta.Member.ParamClause | _: meta.Member.ParamClauseGroup => tree
        .parent.map(getDefnSite)
    case _: meta.Ctor => ctorSite
    case _ => None
  }).getOrElse(defnSite)
}

object Indents {
  implicit lazy val surface: generic.Surface[Indents] = generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[Indents] = generic
    .deriveCodecEx(Indents()).noTypos

  sealed abstract class RelativeToLhs
  object RelativeToLhs {
    case object `match` extends RelativeToLhs
    case object `infix` extends RelativeToLhs

    implicit val reader: ConfCodecEx[RelativeToLhs] = ReaderUtil
      .oneOf[RelativeToLhs](`match`, `infix`)
  }

  sealed abstract class FewerBraces
  object FewerBraces {
    case object never extends FewerBraces
    case object always extends FewerBraces
    case object beforeSelect extends FewerBraces

    implicit val reader: ConfCodecEx[FewerBraces] = ReaderUtil
      .oneOf[FewerBraces](never, always, beforeSelect)
  }

}
