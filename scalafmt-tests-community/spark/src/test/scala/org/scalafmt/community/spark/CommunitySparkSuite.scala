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
      "classic" -> TestStats.Style(expectedStatesVisited = 7822344),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 7822643),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 7794473),
      "fold" -> TestStats.Style(expectedStatesVisited = 12245923),
      "keep" -> TestStats.Style(expectedStatesVisited = 6827133),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 6827259),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 6841859),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 7585082),
      "unfold" -> TestStats.Style(expectedStatesVisited = 8654777),
    ),
  ))

}

class CommunitySpark3_5Suite extends CommunitySparkSuite("spark-3.5") {

  override protected def builds = Seq(getBuild(
    "v3.5.3",
    dialects.Scala213,
    2756,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 8277862),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 8278013),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 8254489),
      "fold" -> TestStats.Style(expectedStatesVisited = 12884359),
      "keep" -> TestStats.Style(expectedStatesVisited = 7218789),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 7218811),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 7237620),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 8057079),
      "unfold" -> TestStats.Style(expectedStatesVisited = 9159243),
    ),
  ))

}
