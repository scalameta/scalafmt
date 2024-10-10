package org.scalafmt.community.common

import org.scalafmt.config.ConfParsed

import scala.meta._

import java.nio.file.FileSystems
import java.nio.file.Path

import metaconfig.Conf

case class CommunityBuild(
    giturl: String,
    commit: String,
    name: String,
    excluded: List[String],
    checkedFiles: Int,
    dialect: sourcecode.Text[Dialect],
    styles: Set[String] = Set.empty,
    stylesIncluded: Boolean = true,
    fileOverride: Option[String] = null,
    statsPerStyle: Map[String, TestStats.Style] = Map.empty,
) {
  private val excludedMatchers = {
    val fs = FileSystems.getDefault
    excluded.map(fs.getPathMatcher)
  }

  def isExcluded(path: Path): Boolean = excludedMatchers
    .exists(p => p.matches(path))

  val fileOverrideConf = fileOverride
    .map(x => ConfParsed.fromString(x).conf.get.asInstanceOf[Conf.Obj])
}
