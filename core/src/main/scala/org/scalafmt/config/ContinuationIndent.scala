package org.scalafmt.config

import metaconfig._

/**
  * @param defnSite indentation around class/def
  * @param callSite indentation around function calls, etc.
  * @param extendSite indentation before `extends`
  */
@DeriveConfDecoder
case class ContinuationIndent(
    callSite: Int = 2,
    defnSite: Int = 4,
    extendSite: Int = 4
)
