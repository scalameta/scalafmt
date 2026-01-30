package org.scalafmt.community.intellij

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunityIntellijScalaSuite(name: String)
    extends CommunityRepoSuite(
      "https://github.com/JetBrains/intellij-scala.git",
      name,
    )

class CommunityIntellijScala_2024_2_Suite
    extends CommunityIntellijScalaSuite("intellij-scala-2024.2") {

  override protected def totalStatesVisited: Option[Int] = Some(59744185)

  override protected def builds = Seq {
    getBuild(
      "2024.2.28",
      dialects.Scala213,
      3469,
      excluded = "glob:**/{testdata,testData,resources}/**" :: Nil,
      fileOverride =
        """|{
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
    )
  }

}

class CommunityIntellijScala_2024_3_Suite
    extends CommunityIntellijScalaSuite("intellij-scala-2024.3") {

  override protected def totalStatesVisited: Option[Int] = Some(59959551)

  override protected def builds = Seq {
    getBuild(
      "2024.3.4",
      dialects.Scala213,
      3475,
      excluded = "glob:**/{testdata,testData,resources}/**" :: Nil,
      fileOverride =
        """|{
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
    )
  }

}
