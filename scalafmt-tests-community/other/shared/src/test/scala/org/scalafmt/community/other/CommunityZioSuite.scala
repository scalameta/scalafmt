package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityZioSuite(name: String)
    extends CommunityRepoSuite("https://github.com/zio/zio.git", name)

class CommunityZio_2_1_Suite extends CommunityZioSuite("zio-2.1") {

  override protected def builds = Seq(getBuild("v2.1.9", dialects.Scala213, 453))

}
