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
      "classic" -> TestStats.Style(expectedStatesVisited = 3651615),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 3653930),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 3692216),
      "fold" -> TestStats.Style(expectedStatesVisited = 6315060),
      "keep" -> TestStats.Style(expectedStatesVisited = 3179974),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 3180034),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 3109365),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 3668368),
      "unfold" -> TestStats.Style(expectedStatesVisited = 4251173),
    ),
  ))

}

class CommunityScala3_3Suite extends CommunityScala3Suite("scala-3.3") {

  override protected def builds = Seq(getBuild(
    "3.3.3",
    dialects.Scala33,
    861,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 3940695),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 3942998),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 3983727),
      "fold" -> TestStats.Style(expectedStatesVisited = 6856356),
      "keep" -> TestStats.Style(expectedStatesVisited = 3420835),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 3420899),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 3352827),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 3945741),
      "unfold" -> TestStats.Style(expectedStatesVisited = 4606935),
    ),
  ))

}
