package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityScalaCliSuite(name: String)
    extends CommunityRepoSuite(
      "https://github.com/VirtusLab/scala-cli.git",
      name,
    )

class CommunityScalaCli1_5Suite
    extends CommunityScalaCliSuite("scala-cli-1.5") {

  override protected def builds = Seq(getBuild(
    "v1.5.0",
    dialects.Scala30,
    589,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 741119),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 741195),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 762587),
      "fold" -> TestStats.Style(expectedStatesVisited = 1281716),
      "keep" -> TestStats.Style(expectedStatesVisited = 641034),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 641114),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 628434),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 697809),
      "unfold" -> TestStats.Style(expectedStatesVisited = 883996),
    ),
  ))

}
