package org.scalafmt

import metaconfig.ConfigReader
import metaconfig.Reader

@ConfigReader
case class ContinuationIndent(callSite: Int, defnSite: Int)

@ConfigReader
case class Spaces(
    beforeContextBoundColon: Boolean,
    afterTripleEquals: Boolean,
    inImportCurlyBraces: Boolean
)
