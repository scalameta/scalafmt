package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityScalazSuite(name: String)
    extends CommunityRepoSuite("https://github.com/scalaz/scalaz.git", name)

class CommunityScalaz_7_3_Suite extends CommunityScalazSuite("scalaz-7.3") {

  override protected def builds = Seq(getBuild(
    "v7.3.8",
    dialects.Scala213,
    420,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 1443217),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 1447606),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 1455766),
      "fold" -> TestStats.Style(expectedStatesVisited = 2393612),
      "keep" -> TestStats.Style(expectedStatesVisited = 1243477),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 1243685),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 1250131),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 1658621),
      "unfold" -> TestStats.Style(expectedStatesVisited = 1764422),
    ),
  ))

}
