import java.io.File
sbtPlugin := true

def assertContentsEqual(file: File, expected: String): Unit = {
  val obtained =
    scala.io.Source.fromFile(file).getLines().mkString("\n")

  if (obtained.trim != expected.trim) {
    val msg =
      s"""Obtained output:
          |$obtained
          |Expected:
          |$expected
          |""".stripMargin
    System.err.println(msg)
    throw new Exception(msg)
  }
}

TaskKey[Unit]("check") := {
  assertContentsEqual(
    new File("src/main/scala/Test.scala"),
    """
      |object Test {
      |  def main(args: Array[String]) {
      |    println("hello")
      |  }
      |}
    """.stripMargin
  )
}
//  Uncomment when #405 is complete
//  assertContentsEqual(
//    new File("project/plugins.sbt"),
//    """
//      |addSbtPlugin(
//      |  "com.geirsson" % "sbt-scalafmt" % System.getProperty("plugin.version"))
//    """.stripMargin
//  )

