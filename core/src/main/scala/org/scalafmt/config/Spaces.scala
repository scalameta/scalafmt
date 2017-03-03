package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class Spaces(
    beforeContextBoundColon: Boolean = false,
    afterTripleEquals: Boolean = false,
    inImportCurlyBraces: Boolean = false,
    beforeSeqWildcard: Boolean = false,
    neverAroundInfixTypes: Seq[String] = Nil
)
