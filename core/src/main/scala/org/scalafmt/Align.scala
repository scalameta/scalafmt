package org.scalafmt

import metaconfig.ConfigReader
import metaconfig.Reader

@ConfigReader
case class Align(
    openParenCallSite: Boolean,
    openParenDefnSite: Boolean,
    mixedOwners: Boolean,
    tokens: Set[AlignToken],
    arrowEnumeratorGenerator: Boolean,
    ifWhileOpenParen: Boolean
) {
  implicit val alignReader: Reader[AlignToken] = ScalafmtStyle.alignReader

}
