package org.scalafmt.community.intellij

import org.scalafmt.community.common.CommunityRepoSuite
import org.scalafmt.community.common.TestStats

import scala.meta._

abstract class CommunityIntellijScalaSuite(name: String)
    extends CommunityRepoSuite(
      "https://github.com/JetBrains/intellij-scala.git",
      name,
    )

class CommunityIntellijScala_2024_2_Suite
    extends CommunityIntellijScalaSuite("intellij-scala-2024.2") {

  override protected def builds = Seq(getBuild(
    "2024.2.28",
    dialects.Scala213,
    3469,
    excluded = "glob:**/{testdata,testData,resources}/**" :: Nil,
    fileOverride = """|{
                      |  "glob:**/sbt/sbt-impl/workspace-entities/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/structure-view/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/repl/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/tasty-reader/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/package-search-client/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/integration/textAnalysis/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/integration/features-trainer/**" {
                      |    runner.dialect = scala33
                      |  }
                      |}
                      |""".stripMargin,
  ))

}

class CommunityIntellijScala_2024_3_Suite
    extends CommunityIntellijScalaSuite("intellij-scala-2024.3") {

  override protected def builds = Seq(getBuild(
    "2024.3.4",
    dialects.Scala213,
    3475,
    excluded = "glob:**/{testdata,testData,resources}/**" :: Nil,
    fileOverride = """|{
                      |  "glob:**/sbt/sbt-impl/workspace-entities/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/structure-view/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/repl/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/tasty-reader/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/package-search-client/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scalac-patches/scalac3-patches/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/integration/textAnalysis/**" {
                      |    runner.dialect = scala33
                      |  }
                      |  "glob:**/scala/integration/features-trainer/**" {
                      |    runner.dialect = scala33
                      |  }
                      |}
                      |""".stripMargin,
    statsPerStyle = Map(
      "classic" -> TestStats.Style(expectedStatesVisited = 5134862),
      "classicWithAlign" -> TestStats.Style(expectedStatesVisited = 5139218),
      "classicWithRewrites" -> TestStats.Style(expectedStatesVisited = 5134172),
      "fold" -> TestStats.Style(expectedStatesVisited = 7953034),
      "keep" -> TestStats.Style(expectedStatesVisited = 4737913),
      "keepWithAlign" -> TestStats.Style(expectedStatesVisited = 4739065),
      "keepWithRewrites" -> TestStats.Style(expectedStatesVisited = 4748954),
      "keepWithScalaJS" -> TestStats.Style(expectedStatesVisited = 5167360),
      "unfold" -> TestStats.Style(expectedStatesVisited = 5611808),
    ),
  ))

}
