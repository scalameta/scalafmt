package org.scalafmt.config

import metaconfig._

/**
  * @param defnSite indentation around class/def
  * @param callSite indentation around function calls, etc.
  * @param extendSite indentation before `extends`
  * @param typeAnnSite indentation before return type and type annotations
  */
case class ContinuationIndent(
    callSite: Int = 2,
    defnSite: Int = 4,
    extendSite: Int = 4,
    typeAnnSite: Int = 2
) {
  implicit val reader: ConfDecoder[ContinuationIndent] =
    generic.deriveDecoder(this).noTypos
}

object ContinuationIndent {
  implicit lazy val surface: generic.Surface[ContinuationIndent] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[ContinuationIndent] =
    generic.deriveEncoder
}
