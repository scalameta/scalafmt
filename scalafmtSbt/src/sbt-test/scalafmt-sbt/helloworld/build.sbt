import java.io.File

lazy val root = project
  .in(file("."))
  .aggregate(
    p1,
    p2,
    p3
  )

lazy val p1 = project.settings(scalaVersion := "2.10.5")
lazy val p2 = project.settings(scalaVersion := "2.11.8")
lazy val p3 = project.settings(scalaVersion := "2.12.1")

def assertContentsEqual(file: File, expected: String): Unit = {
  val obtained =
    scala.io.Source.fromFile(file).getLines().mkString("\n")

  if (obtained.trim != expected.trim) {
    val msg =
      s"""File: $file
         |Obtained output:
         |$obtained
         |Expected:
         |$expected
         |""".stripMargin
    System.err.println(msg)
    throw new Exception(msg)
  }
}

TaskKey[Unit]("setupGitRepo") := {
  import sys.process._
  Seq("git", "init").!!
  Seq("git", "config", "user.email", "a@a.is").!!
  Seq("git", "config", "user.name", "a").!!
  Seq("git", "add", ".").!!
  Seq("git", "commit", "-m", "wip").!!
}

TaskKey[Unit]("check") := {
  (1 to 3).foreach { i =>
    val expected =
      """
        |object Test {
        |  List(
        |    1, // comment
        |    2
        |  )
        |}
        """.stripMargin
    val expected2 = expected.replaceFirst("Test", "MainTest")
    assertContentsEqual(file(s"p$i/src/main/scala/Test.scala"), expected)
    assertContentsEqual(file(s"p$i/src/test/scala/MainTest.scala"), expected2)
  }
  assertContentsEqual(
    new File("project/plugins.sbt"),
    """
      |addSbtPlugin(
      |  "com.geirsson" % "sbt-scalafmt" % System.getProperty("plugin.version")
      |)
    """.stripMargin
  )
  assertContentsEqual(
    new File("target/src_managed/Generated.scala"),
    """object      DontFormatMe   {    println(1)    }"""
  )
}
