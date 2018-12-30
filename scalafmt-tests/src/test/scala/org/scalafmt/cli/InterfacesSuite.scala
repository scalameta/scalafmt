package org.scalafmt.cli

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.attribute.FileTime
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtBuilder
import org.scalatest.BeforeAndAfterEach
import org.scalatest.FunSuite
import scala.meta.testkit.DiffAssertions
import scala.meta.testkit.StringFS

class InterfacesSuite
    extends FunSuite
    with DiffAssertions
    with BeforeAndAfterEach {
  val out = new ByteArrayOutputStream()
  var reporter = new ScalafmtReporterImpl(new PrintStream(out))

  def newBuilder(): ScalafmtBuilder =
    Scalafmt
      .newBuilder(this.getClass.getClassLoader)
      .withReporter(reporter)

  override def beforeEach(): Unit = {
    out.reset()
  }

  test("basic") {
    val scalafmt = newBuilder().create()
    val obtained = scalafmt.format(Paths.get("foo.scala"), "object  Foo {  }")
    assert(obtained == "object Foo {}\n")
  }

  test("config") {
    val config = Files.createTempFile("scalafmt", ".scalafmt.conf")
    Files.write(config, "maxColumn=12".getBytes(StandardCharsets.UTF_8))
    val scalafmt = newBuilder().withConfig(config).create()
    val file = Paths.get("foo.scala")
    val code = "object  Foo {  val x = 1 }"
    assertNoDiff(
      scalafmt.format(file, code),
      """object Foo {
        |  val x = 1
        |}
        |""".stripMargin
    )
    out.reset()
    assert(out.toString().isEmpty)
    Files.write(config, "maxColumn=4".getBytes(StandardCharsets.UTF_8))
    // Force timestamp to change, this unit test runs so fast it becomes
    // flaky without the following line.
    Files.setLastModifiedTime(config, FileTime.fromMillis(10))
    assertNoDiff(
      scalafmt.format(file, code),
      """object Foo {
        |  val x =
        |    1
        |}
        |""".stripMargin
    )
    val reported = out.toString()
    assert(reported.contains("parsed config"))
    assert(reported.contains(".scalafmt.conf"))
  }
  test("filters") {
    val root = StringFS.fromString(
      """
        |/.scalafmt.conf
        |project.includeFilters = [
        |  ".*Spec\\.scala$"
        |]
        |project.excludeFilters = [
        |  "UserSpec\\.scala$"
        |]
        |/Main.scala
        |object   Main
        |/UserSpec.scala
        |object   UserSpec
        |/ResourceSpec.scala
        |object   ResourceSpec
      """.stripMargin
    )
    val scalafmt = newBuilder()
      .withConfig(root.resolve(".scalafmt.conf").toNIO)
      .create()
    import scala.collection.JavaConverters._
    Files.list(root.toNIO).iterator().asScala.foreach { file =>
      val text = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
      val out = scalafmt.format(file, text)
      Files.write(file, out.getBytes(StandardCharsets.UTF_8))
    }
    assertNoDiff(
      StringFS.asString(root),
      """
        |/.scalafmt.conf
        |project.includeFilters = [
        |  ".*Spec\\.scala$"
        |]
        |project.excludeFilters = [
        |  "UserSpec\\.scala$"
        |]
        |/Main.scala
        |object   Main
        |/ResourceSpec.scala
        |object ResourceSpec
        |
        |/UserSpec.scala
        |object   UserSpec
        |""".stripMargin
    )
    val reported = out
      .toString()
      .replaceAllLiterally(root.toString(), "")
      .lines
      .toList
      .sorted
      .mkString("\n")
    assertNoDiff(
      reported,
      """
        |file excluded: /.scalafmt.conf
        |file excluded: /Main.scala
        |file excluded: /UserSpec.scala
        |parsed config: /.scalafmt.conf
      """.stripMargin
    )

  }

}
