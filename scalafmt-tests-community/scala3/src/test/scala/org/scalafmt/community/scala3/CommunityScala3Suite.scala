package org.scalafmt.community.scala3

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityScala3Suite(name: String)
    extends CommunityRepoSuite("https://github.com/scala/scala3.git", name)

class CommunityScala3_2Suite extends CommunityScala3Suite("scala-3.2") {

  override protected def builds = Seq(getBuild(
    "3.2.2",
    dialects.Scala32,
    791,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 3550612),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 3552306),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 3558246),
      "fold" -> TestStats.Style(expectedStatesVisited = 5817619),
      "keep" -> TestStats.Style(expectedStatesVisited = 3148692),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 3148747),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 3079850),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 3631800),
      "unfold" -> TestStats.Style(expectedStatesVisited = 3882869),
    ),
  ))

}

class CommunityScala3_3Suite extends CommunityScala3Suite("scala-3.3") {

  override protected def builds = Seq(getBuild(
    "3.3.3",
    dialects.Scala33,
    861,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 3829673),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 3831499),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 3839204),
      "fold" -> TestStats.Style(expectedStatesVisited = 6310491),
      "keep" -> TestStats.Style(expectedStatesVisited = 3388129),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 3388188),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 3321872),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 3907628),
      "unfold" -> TestStats.Style(expectedStatesVisited = 4201515),
    ),
  ))

}
