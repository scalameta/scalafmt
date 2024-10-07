package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

class CommunityMunitSuite
    extends CommunityRepoSuite("https://github.com/scalameta/munit.git", "munit") {

  override protected def builds = Seq(getBuild("v1.0.1", dialects.Scala213, 109))

}
