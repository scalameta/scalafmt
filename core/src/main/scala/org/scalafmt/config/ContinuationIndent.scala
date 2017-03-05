package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * @param defnSite indentation around class/def
  * @param callSite indentation around function calls, etc.
  * @param extendSite indentation before `extends`
  */
@ConfigReader
case class ContinuationIndent(
    callSite: Int = 2,
    defnSite: Int = 4,
    extendSite: Int = 4
)
