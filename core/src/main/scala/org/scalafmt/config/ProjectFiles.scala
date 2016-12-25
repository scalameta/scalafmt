package org.scalafmt.config

import metaconfig.ConfigReader
import org.scalafmt.util.OsSpecific

@ConfigReader
case class ProjectFiles(
    git: Boolean = false,
    files: Seq[String] = Nil,
    includeFilters: Seq[String] = Seq(".*\\.scala", ".*\\.sbt"),
    excludeFilters: Seq[String] = Nil
) {
  lazy val matcher: FilterMatcher =
    FilterMatcher(includeFilters.map(OsSpecific.fixSeparatorsInPathPattern),
                  excludeFilters.map(OsSpecific.fixSeparatorsInPathPattern))
}
