package org.scalafmt.config

import metaconfig._

/** @param main the primary indentation used in the code
  * @param significant the indentation used when optional braces are omitted
  * @param defnSite indentation around class/def
  * @param ctorSite indentation around class constructor parameters
  * @param caseSite indentation for case values before arrow
  * @param callSite indentation around function calls, etc.
  * @param extendSite indentation before `extends`
  * @param withSiteRelativeToExtends additional indentation before `with`
  * @param commaSiteRelativeToExtends additional indentation before in the line after extends with a comma
  */
case class Indents(
    main: Int = 2,
    private[config] val significant: Option[Int] = None,
    callSite: Int = 2,
    ctrlSite: Option[Int] = None,
    defnSite: Int = 4,
    caseSite: Int = 4,
    private[config] val ctorSite: Option[Int] = None,
    @annotation.ExtraName("deriveSite")
    extendSite: Int = 4,
    withSiteRelativeToExtends: Int = 0,
    commaSiteRelativeToExtends: Int = 2
) {
  lazy val getSignificant = significant.getOrElse(main)

  def getDefnSite(tree: meta.Tree): Int =
    (tree match {
      case _: meta.Ctor => ctorSite
      case _ => None
    }).getOrElse(defnSite)
}

object Indents {
  implicit lazy val surface: generic.Surface[Indents] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[Indents] =
    generic.deriveCodecEx(Indents()).noTypos
}
