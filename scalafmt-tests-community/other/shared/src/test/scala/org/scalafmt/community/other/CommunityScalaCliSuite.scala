package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityScalaCliSuite(name: String)
    extends CommunityRepoSuite(
      "https://github.com/VirtusLab/scala-cli.git",
      name,
    )

class CommunityScalaCli1_5Suite
    extends CommunityScalaCliSuite("scala-cli-1.5") {

  override protected def builds = Seq(getBuild("v1.5.0", dialects.Scala30, 589))

}
