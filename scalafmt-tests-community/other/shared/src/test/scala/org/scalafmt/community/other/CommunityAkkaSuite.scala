package org.scalafmt.community.other

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityAkkaSuite(name: String)
    extends CommunityRepoSuite("https://github.com/akka/akka.git", name)

class CommunityAkka_2_9_Suite extends CommunityAkkaSuite("akka-2.9") {

  override protected def builds = Seq(getBuild(
    "v2.9.6",
    dialects.Scala213,
    1401,
    excluded = "glob:**/scripts/*" :: Nil,
  ))

}
