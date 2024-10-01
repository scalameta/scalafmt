package org.scalafmt.community.common

import scala.meta._

import java.nio.file.FileSystems
import java.nio.file.Path

case class CommunityBuild(
    giturl: String,
    commit: String,
    name: String,
    excluded: List[String],
    checkedFiles: Int,
    dialect: sourcecode.Text[Dialect],
    styles: Set[String] = Set.empty,
    stylesIncluded: Boolean = true,
) {
  private val excludedMatchers = {
    val fs = FileSystems.getDefault
    excluded.map(fs.getPathMatcher)
  }

  def isExcluded(path: Path): Boolean = excludedMatchers
    .exists(p => p.matches(path))
}
