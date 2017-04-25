package org.scalafmt.config

import metaconfig._

/**
  *
  * @param callSite if true If true, will fit as many arguments on each line,
  *                         only breaking at commas. If false, a function
  *                         call's arguments will either be all on the same
  *                         line or will have one line each.
  * @param defnSite Same as [[callSite]], except for definition site.
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
  * @param parentConstructors Parent constructors are C and D in
  *                           "class A extends B with C and D". If true,
  *                           scalafmt will fit as many parent constructors
  *                           on a single line. If false, each parent
  *                           constructor gets its own line.
  *
  */
@DeriveConfDecoder
case class BinPack(
    callSite: Boolean = false,
    defnSite: Boolean = false,
    lambdaParameter: Boolean = false,
    parentConstructors: Boolean = false,
    literalArgumentLists: Boolean = true,
    literalsMinArgCount: Int = 5,
    literalsInclude: Seq[String] = Seq(".*"),
    literalsExclude: Seq[String] = Seq("String", "Term.Name")
) {
  def literalsRegex: FilterMatcher =
    FilterMatcher(literalsInclude, literalsExclude)
}
