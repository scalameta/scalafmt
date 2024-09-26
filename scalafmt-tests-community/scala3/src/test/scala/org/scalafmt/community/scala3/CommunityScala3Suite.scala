package org.scalafmt.community.scala3

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityScala3Suite(name: String)
    extends CommunityRepoSuite("https://github.com/scala/scala3.git", name)

class CommunityScala3_0Suite extends CommunityScala3Suite("scala-3.0") {

  override protected def builds = Seq(getBuild("3.0.2", dialects.Scala30, 698))

}

class CommunityScala3_1Suite extends CommunityScala3Suite("scala-3.1") {

  override protected def builds = Seq(getBuild("3.1.3", dialects.Scala31, 747))

}

class CommunityScala3_2Suite extends CommunityScala3Suite("scala-3.2") {

  override protected def builds = Seq(getBuild("3.2.2", dialects.Scala32, 791))

}

class CommunityScala3_3Suite extends CommunityScala3Suite("scala-3.3") {

  override protected def builds = Seq(getBuild("3.3.3", dialects.Scala33, 861))

}
