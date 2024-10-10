package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityZioSuite(name: String)
    extends CommunityRepoSuite("https://github.com/zio/zio.git", name)

class CommunityZio_2_1_Suite extends CommunityZioSuite("zio-2.1") {

  override protected def builds = Seq(getBuild(
    "v2.1.9",
    dialects.Scala213,
    453,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 1987103),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 2054863),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 2077626),
      "fold" -> TestStats.Style(expectedStatesVisited = 4270654),
      "keep" -> TestStats.Style(expectedStatesVisited = 1690910),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 1711594),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 1705443),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 2036604),
      "unfold" -> TestStats.Style(expectedStatesVisited = 2540862),
    ),
  ))

}
