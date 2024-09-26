package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityScalaCliSuite(name: String)
    extends CommunityRepoSuite(
      "https://github.com/VirtusLab/scala-cli.git",
      name,
    )

class CommunityScalaCli1_2Suite
    extends CommunityScalaCliSuite("scala-cli-1.2") {

  override protected def builds = Seq(getBuild("v1.2.2", dialects.Scala30, 581))

}

class CommunityScalaCli1_3Suite
    extends CommunityScalaCliSuite("scala-cli-1.3") {

  override protected def builds = Seq(getBuild("v1.3.2", dialects.Scala30, 582))

}

class CommunityScalaCli1_4Suite
    extends CommunityScalaCliSuite("scala-cli-1.4") {

  override protected def builds = Seq(getBuild("v1.4.3", dialects.Scala30, 586))

}

class CommunityScalaCli1_5Suite
    extends CommunityScalaCliSuite("scala-cli-1.5") {

  override protected def builds = Seq(getBuild("v1.5.0", dialects.Scala30, 589))

}
