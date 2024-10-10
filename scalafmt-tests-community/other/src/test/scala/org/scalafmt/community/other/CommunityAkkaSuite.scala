package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityAkkaSuite(name: String)
    extends CommunityRepoSuite("https://github.com/akka/akka.git", name)

class CommunityAkka_2_9_Suite extends CommunityAkkaSuite("akka-2.9") {

  override protected def builds = Seq(getBuild(
    "v2.9.6",
    dialects.Scala213,
    1401,
    excluded = "glob:**/scripts/*" :: Nil,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 3512524),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 3512674),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 3538477),
      "fold" -> TestStats.Style(expectedStatesVisited = 5927363),
      "keep" -> TestStats.Style(expectedStatesVisited = 3107264),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 3107259),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 3121296),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 3428752),
      "unfold" -> TestStats.Style(expectedStatesVisited = 4117811),
    ),
  ))

}
