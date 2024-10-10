package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityScalaJsSuite(name: String)
    extends CommunityRepoSuite("https://github.com/scala-js/scala-js.git", name)

class CommunityScalaJs1_17Suite extends CommunityScalaJsSuite("scala-js-1.17") {

  override protected def builds = Seq(getBuild(
    "v1.17.0",
    dialects.Scala213,
    787,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 2144529),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 2145182),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 2155115),
      "fold" -> TestStats.Style(expectedStatesVisited = 3332603),
      "keep" -> TestStats.Style(expectedStatesVisited = 1939787),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 1939865),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 1945861),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 2250205),
      "unfold" -> TestStats.Style(expectedStatesVisited = 2523432),
    ),
  ))

}
