package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityPlayFrameworkSuite(name: String)
    extends CommunityRepoSuite(
      "https://github.com/playframework/playframework.git",
      name,
    )

class CommunityPlayFramework_3_0_Suite
    extends CommunityPlayFrameworkSuite("playframework-3.0") {

  override protected def builds = Seq(getBuild(
    "3.0.5",
    dialects.Scala213,
    454,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 1000528),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 1001078),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 1014356),
      "fold" -> TestStats.Style(expectedStatesVisited = 1539344),
      "keep" -> TestStats.Style(expectedStatesVisited = 788895),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 788945),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 798416),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 885021),
      "unfold" -> TestStats.Style(expectedStatesVisited = 1069374),
    ),
  ))

}
