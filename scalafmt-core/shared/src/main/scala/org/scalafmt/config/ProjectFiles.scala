package org.scalafmt.config

import metaconfig._
import org.scalafmt.util.OsSpecific

case class ProjectFiles(
    git: Boolean = false,
    files: Seq[String] = Nil,
    includeFilters: Seq[String] = Seq(".*\\.scala$", ".*\\.sbt$", ".*\\.sc$"),
    excludeFilters: Seq[String] = Nil
) {
  lazy val matcher: FilterMatcher =
    FilterMatcher(
      includeFilters.map(OsSpecific.fixSeparatorsInPathPattern),
      excludeFilters.map(OsSpecific.fixSeparatorsInPathPattern))
}
