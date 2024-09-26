package org.scalafmt.community

import scala.meta._

abstract class CommunityRepoSuite(giturl: String, name: String)
    extends CommunitySuite {

  protected def getBuild(
      ref: String,
      dialect: sourcecode.Text[Dialect],
      files: Int,
  ) = CommunityBuild(giturl, ref, name, Nil, files, dialect)

}
