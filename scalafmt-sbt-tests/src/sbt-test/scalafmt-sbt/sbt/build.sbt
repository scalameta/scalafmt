import java.io.File

fork in ThisBuild := true

lazy val p123 = project
  .in(file("."))
  .aggregate(
    p1,
    p2,
    p3
  )

lazy val p1 = project.settings(
  scalaVersion := "2.10.5"
)
lazy val p2 = project.settings(
  scalaVersion := "2.11.8"
)
lazy val p3 = project.settings(
  scalaVersion := "2.12.1"
)
lazy val p4 = project.settings(
  scalaVersion := "2.12.1"
)
lazy val p5 = project.settings(
  scalaVersion := "2.12.1",
  scalafmtOnCompile := true
)
lazy val p6 = project.settings(
  scalaVersion := "2.12.1",
  scalafmtConfig := file(".scalafmt6.conf")
)

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

TaskKey[Unit]("check") := {
  (1 to 4).foreach { i =>
    val expectedTest =
      """
        |object Test {
        |  foo(
        |    a, // comment
        |    b
        |  )
        |}
        """.stripMargin
    val expectedMainTest = expectedTest.replaceFirst("Test", "MainTest")
    assertContentsEqual(
      file(s"p$i/src/main/scala/Test.scala"),
      expectedTest
    )
    assertContentsEqual(
      file(s"p$i/src/test/scala/MainTest.scala"),
      expectedMainTest
    )
  }
  assertContentsEqual(
    file(s"p5/src/main/scala/Test.scala"),
    """
      |object Test {
      |  def foo(a: Int, // comment
      |          b: Double) = ???
      |}
    """.stripMargin
  )
  assertContentsEqual(
    file(s"p5/src/test/scala/MainTest.scala"),
    """
      |object MainTest {
      |  def foo(a: Int, // comment
      |          b: Double) = ???
      |}
    """.stripMargin
  )
  assertContentsEqual(
    file(s"p6/src/main/scala/Test.scala"),
    """
      |object Test {
      |  foo(a, // comment
      |      b)
      |}
    """.stripMargin
  )
  assertContentsEqual(
    file(s"p6/src/test/scala/MainTest.scala"),
    """
      |object MainTest {
      |  foo(a, // comment
      |      b)
      |}
    """.stripMargin
  )

  assertContentsEqual(
    file("project/plugins.sbt"),
    """
      |addSbtPlugin(
      |  "com.geirsson" % "sbt-scalafmt" % System.getProperty("plugin.version")
      |)
    """.stripMargin
  )
}
