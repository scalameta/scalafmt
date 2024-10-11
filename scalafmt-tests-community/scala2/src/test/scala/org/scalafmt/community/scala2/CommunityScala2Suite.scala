package org.scalafmt.community.scala2

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityScala2Suite(name: String)
    extends CommunityRepoSuite("https://github.com/scala/scala.git", name)

class CommunityScala2_12Suite extends CommunityScala2Suite("scala-2.12") {

  override protected def builds = Seq(getBuild(
    "v2.12.20",
    dialects.Scala212,
    1277,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 3742070),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 3743075),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 3782691),
      "fold" -> TestStats.Style(expectedStatesVisited = 5833324),
      "keep" -> TestStats.Style(expectedStatesVisited = 3511074),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 3511201),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 3557273),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 3976667),
      "unfold" -> TestStats.Style(expectedStatesVisited = 4192369),
    ),
  ))

}

class CommunityScala2_13Suite extends CommunityScala2Suite("scala-2.13") {

  override protected def builds = Seq(getBuild(
    "v2.13.14",
    dialects.Scala213,
    1287,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 4633753),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 4636193),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 4666475),
      "fold" -> TestStats.Style(expectedStatesVisited = 7586002),
      "keep" -> TestStats.Style(expectedStatesVisited = 4329252),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 4329386),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 4373731),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 4928298),
      "unfold" -> TestStats.Style(expectedStatesVisited = 5125363),
    ),
  ))

}
