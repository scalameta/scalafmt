package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

class CommunityMunitSuite
    extends CommunityRepoSuite("https://github.com/scalameta/munit.git", "munit") {

  override protected def builds = Seq(getBuild(
    "v1.0.1",
    dialects.Scala213,
    109,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 62866),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 62878),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 62798),
      "fold" -> TestStats.Style(expectedStatesVisited = 111030),
      "keep" -> TestStats.Style(expectedStatesVisited = 58284),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 58284),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 57904),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 62107),
      "unfold" -> TestStats.Style(expectedStatesVisited = 80896),
    ),
  ))

}
