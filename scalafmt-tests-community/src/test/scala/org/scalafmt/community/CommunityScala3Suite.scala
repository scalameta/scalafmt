package org.scalafmt.community

import scala.meta._

abstract class CommunityScala3Suite extends CommunitySuite {

  protected def getBuild(
      name: String,
      ref: String,
      dialect: sourcecode.Text[Dialect],
      files: Int,
  ) = CommunityBuild(
    "https://github.com/scala/scala3.git",
    ref,
    name,
    Nil,
    files,
    dialect,
  )

}

class CommunityScala3_0Suite extends CommunityScala3Suite {

  override protected def builds =
    Seq(getBuild("scala-3.0", "3.0.2", dialects.Scala30, 698))

}

class CommunityScala3_1Suite extends CommunityScala3Suite {

  override protected def builds =
    Seq(getBuild("scala-3.1", "3.1.3", dialects.Scala31, 747))

}

class CommunityScala3_2Suite extends CommunityScala3Suite {

  override protected def builds =
    Seq(getBuild("scala-3.2", "3.2.2", dialects.Scala32, 791))

}

class CommunityScala3_3Suite extends CommunityScala3Suite {

  override protected def builds =
    Seq(getBuild("scala-3.3", "3.3.3", dialects.Scala33, 861))

}
