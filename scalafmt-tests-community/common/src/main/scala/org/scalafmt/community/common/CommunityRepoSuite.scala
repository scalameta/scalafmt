package org.scalafmt.community.common

import org.scalafmt.config.ScalafmtConfig

import scala.meta._

abstract class CommunityRepoSuite(giturl: String, name: String)
    extends CommunitySuite {

  protected def getBuild(
      ref: String,
      dialect: sourcecode.Text[Dialect],
      files: Int,
      excluded: List[String] = Nil,
      fileOverride: String = null,
      styles: Seq[sourcecode.Text[ScalafmtConfig]] = Seq.empty,
      statsPerStyle: Map[String, TestStats.Style] = Map.empty,
      statsAllStyles: Option[TestStats.Style] = None,
      stylesIncluded: Boolean = true,
  ) = CommunityBuild(
    giturl,
    ref,
    name,
    excluded,
    files,
    dialect,
    styles = styles.map(_.source).map(x => x.substring(1 + x.lastIndexOf('.')))
      .toSet,
    fileOverride = Option(fileOverride),
    statsPerStyle = statsPerStyle,
    statsAllStyles = statsAllStyles,
    stylesIncluded = stylesIncluded,
  )

}
