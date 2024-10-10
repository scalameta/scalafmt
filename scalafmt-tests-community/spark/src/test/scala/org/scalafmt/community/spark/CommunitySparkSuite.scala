package org.scalafmt.community.spark

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunitySparkSuite(name: String)
    extends CommunityRepoSuite("https://github.com/apache/spark.git", name)

class CommunitySpark3_4Suite extends CommunitySparkSuite("spark-3.4") {

  override protected def builds = Seq(getBuild(
    "v3.4.1",
    dialects.Scala213,
    2585,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 7904959),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 7905199),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 7920730),
      "fold" -> TestStats.Style(expectedStatesVisited = 13323293),
      "keep" -> TestStats.Style(expectedStatesVisited = 6865865),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 6865991),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 6885784),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 7629048),
      "unfold" -> TestStats.Style(expectedStatesVisited = 9725978),
    ),
  ))

}

class CommunitySpark3_5Suite extends CommunitySparkSuite("spark-3.5") {

  override protected def builds = Seq(getBuild(
    "v3.5.3",
    dialects.Scala213,
    2756,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 8363446),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 8363584),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 8386960),
      "fold" -> TestStats.Style(expectedStatesVisited = 13972239),
      "keep" -> TestStats.Style(expectedStatesVisited = 7258827),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 7258849),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 7283864),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 8102571),
      "unfold" -> TestStats.Style(expectedStatesVisited = 10255444),
    ),
  ))

}
