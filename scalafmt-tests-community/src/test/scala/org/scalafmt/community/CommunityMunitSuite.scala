package org.scalafmt.community

import scala.meta._

class CommunityMunitSuite extends CommunitySuite {

  override protected def builds = Seq(
    getBuild("v1.0.1", dialects.Scala213, 109),
    // latest commit from 30.03.2021
    getBuild("06346adfe3519c384201eec531762dad2f4843dc", dialects.Scala213, 102),
  )

  private def getBuild(
      ref: String,
      dialect: sourcecode.Text[Dialect],
      files: Int,
  ) = CommunityBuild(
    "https://github.com/scalameta/munit.git",
    ref,
    "munit",
    Nil,
    files,
    dialect,
  )

}
