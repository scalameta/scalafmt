package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * @param lambdaParameter
  *   If true, vertically aligns by the opening parens of lambda. See
  *   file OpenParenLambda{True,False}.stat.
  *   If false, treats lambda parameter lists like regular definition lists.
  */
@ConfigReader
case class BinPack(
    callSite: Boolean = false,
    defnSite: Boolean = false,
    lambdaParameter: Boolean = true,
    parentConstructors: Boolean = false,
    literalArgumentLists: Boolean = true,
    literalsMinArgCount: Int = 5,
    literalsInclude: Seq[String] = Seq(".*"),
    literalsExclude: Seq[String] = Seq("String")
) {
  def literalsRegex: FilterMatcher =
    FilterMatcher(literalsInclude, literalsExclude)
}
