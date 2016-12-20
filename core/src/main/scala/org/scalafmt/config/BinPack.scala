package org.scalafmt.config

import scala.util.matching.Regex

import metaconfig.ConfigReader

@ConfigReader
case class BinPack(
    callSite: Boolean = false,
    defnSite: Boolean = false,
    parentConstructors: Boolean = false,
    literalArgumentLists: Boolean = true,
    literalsInclude: Seq[String] = Seq(".*"),
    literalsExclude: Seq[String] = Seq("String")
) {
  def literalsRegex: FilterMatcher =
    FilterMatcher(literalsInclude, literalsExclude)
}
