package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * @param lambdaParameter
  *   If true, vertically aligns by the opening parens of lambda. See
  *   file OpenParenLambda{True,False}.stat.
  *   If false, treats lambda parameter lists like regular definition lists.
  * @param literalArgumentLists
  *   If true, automatically sets the style to bin-pack for argument lists
  *   that only consist of literals.
  * @param literalsMinArgCount Argument list must be longer than this setting
  *                            to be eligible for [[literalArgumentLists]].
  * @param literalsInclude Regexes for literal type names. For example, "Int"
  *                        or "Byte".
  * @param literalsExclude Regexes for literal to exclude from [[literalArgumentLists]].
  *
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
