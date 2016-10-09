package org.scalafmt.config

import metaconfig.ConfigReader
import metaconfig.Reader

@ConfigReader
case class Align(
    openParenCallSite: Boolean = true,
    openParenDefnSite: Boolean = true,
    mixedOwners: Boolean = false,
    tokens: Set[AlignToken] = Set.empty[AlignToken],
    arrowEnumeratorGenerator: Boolean = false,
    ifWhileOpenParen: Boolean = true
) {
  implicit val alignReader: Reader[Set[AlignToken]] = ScalafmtConfig.alignReader(tokens)
}
