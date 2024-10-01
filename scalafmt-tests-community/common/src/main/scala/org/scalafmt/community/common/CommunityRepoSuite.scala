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
  ) = CommunityBuild(
    giturl,
    ref,
    name,
    excluded,
    files,
    dialect,
    fileOverride = Option(fileOverride),
  )

}
