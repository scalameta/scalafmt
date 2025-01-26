package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityScalaJsSuite(name: String)
    extends CommunityRepoSuite("https://github.com/scala-js/scala-js.git", name)

class CommunityScalaJs1_17Suite extends CommunityScalaJsSuite("scala-js-1.17") {

  override protected def builds = Seq(getBuild("v1.17.0", dialects.Scala213, 787))

}
