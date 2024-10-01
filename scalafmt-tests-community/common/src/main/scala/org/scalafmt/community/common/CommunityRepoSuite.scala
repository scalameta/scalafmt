package org.scalafmt.community.common

import scala.meta._

abstract class CommunityRepoSuite(giturl: String, name: String)
    extends CommunitySuite {

  protected def getBuild(
      ref: String,
      dialect: sourcecode.Text[Dialect],
      files: Int,
      excluded: List[String] = Nil,
  ) = CommunityBuild(giturl, ref, name, excluded, files, dialect)

}
