package org.scalafmt.community.scala3

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityScala3Suite(name: String)
    extends CommunityRepoSuite("https://github.com/scala/scala3.git", name)

class CommunityScala3_2Suite extends CommunityScala3Suite("scala-3.2") {

  override protected def totalStatesVisited: Option[Int] = Some(39499503)

  override protected def builds = Seq(getBuild("3.2.2", dialects.Scala32, 791))

}

class CommunityScala3_3Suite extends CommunityScala3Suite("scala-3.3") {

  override protected def totalStatesVisited: Option[Int] = Some(42639665)

  override protected def builds = Seq(getBuild("3.3.3", dialects.Scala33, 861))

}
