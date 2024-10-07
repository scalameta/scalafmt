package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityPlayFrameworkSuite(name: String)
    extends CommunityRepoSuite(
      "https://github.com/playframework/playframework.git",
      name,
    )

class CommunityPlayFramework_3_0_Suite
    extends CommunityPlayFrameworkSuite("playframework-3.0") {

  override protected def builds = Seq(getBuild("3.0.5", dialects.Scala213, 454))

}
