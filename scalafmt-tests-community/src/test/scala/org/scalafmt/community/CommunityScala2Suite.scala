package org.scalafmt.community

import scala.meta._

abstract class CommunityScala2Suite extends CommunitySuite {

  protected def getBuild(
      name: String,
      ref: String,
      dialect: sourcecode.Text[Dialect],
      files: Int,
  ): CommunityBuild = CommunityBuild(
    "https://github.com/scala/scala.git",
    ref,
    name,
    Nil,
    files,
    dialect,
  )

}

class CommunityScala2_11Suite extends CommunityScala2Suite {

  override protected def builds =
    Seq(getBuild("scala-2.11", "v2.11.12", dialects.Scala211, 1286))

}

class CommunityScala2_12Suite extends CommunityScala2Suite {

  override protected def builds =
    Seq(getBuild("scala-2.12", "v2.12.20", dialects.Scala212, 1277))

}

class CommunityScala2_13Suite extends CommunityScala2Suite {

  override protected def builds =
    Seq(getBuild("scala-2.13", "v2.13.14", dialects.Scala213, 1287))

}
