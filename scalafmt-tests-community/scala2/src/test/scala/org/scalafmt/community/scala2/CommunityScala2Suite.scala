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
      "classic" -> TestStats.Style(expectedStatesVisited = 3805713),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 3806692),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 3854699),
      "fold" -> TestStats.Style(expectedStatesVisited = 6311579),
      "keep" -> TestStats.Style(expectedStatesVisited = 3546688),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 3546807),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 3595114),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 4017088),
      "unfold" -> TestStats.Style(expectedStatesVisited = 4538915),
    ),
  ))

}

class CommunityScala2_13Suite extends CommunityScala2Suite("scala-2.13") {

  override protected def builds = Seq(getBuild(
    "v2.13.14",
    dialects.Scala213,
    1287,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 4724003),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 4726470),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 4765754),
      "fold" -> TestStats.Style(expectedStatesVisited = 8271306),
      "keep" -> TestStats.Style(expectedStatesVisited = 4380943),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 4381096),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 4428150),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 4985371),
      "unfold" -> TestStats.Style(expectedStatesVisited = 5556752),
    ),
  ))

}
