package org.scalafmt.community.common

import scala.meta._

abstract class CommunityRepoSuite(giturl: String, name: String)
    extends CommunitySuite {

  protected def getBuild(
      ref: String,
      dialect: sourcecode.Text[Dialect],
      files: Int,
      excluded: List[String] = Nil,
      fileOverride: String = null,
      styles: Set[String] = Set.empty,
      statsPerStyle: Map[String, TestStats.Style] = Map.empty,
  ) = CommunityBuild(
    giturl,
    ref,
    name,
    excluded,
    files,
    dialect,
    styles = styles,
    fileOverride = Option(fileOverride),
    statsPerStyle = statsPerStyle,
  )

}
