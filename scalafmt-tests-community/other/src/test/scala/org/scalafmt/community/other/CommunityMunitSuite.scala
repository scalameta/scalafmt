package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

class CommunityMunitSuite
    extends CommunityRepoSuite("https://github.com/scalameta/munit.git", "munit") {

  override protected def builds = Seq(
    getBuild("v1.0.1", dialects.Scala213, 109),
    // latest commit from 30.03.2021
    getBuild("06346adfe3519c384201eec531762dad2f4843dc", dialects.Scala213, 102),
  )

}
