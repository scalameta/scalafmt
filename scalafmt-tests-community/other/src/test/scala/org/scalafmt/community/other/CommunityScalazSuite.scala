package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityScalazSuite(name: String)
    extends CommunityRepoSuite("https://github.com/scalaz/scalaz.git", name)

class CommunityScalaz_7_3_Suite extends CommunityScalazSuite("scalaz-7.3") {

  override protected def builds = Seq(getBuild("v7.3.8", dialects.Scala213, 420))

}
