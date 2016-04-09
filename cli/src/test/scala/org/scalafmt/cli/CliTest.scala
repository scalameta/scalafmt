package org.scalafmt.cli

import java.io.File
import java.nio.file.Files

import org.scalafmt.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite

class CliTest extends FunSuite with DiffAssertions {
  import org.scalafmt.util.LoggerOps._
  val unformatted = """
                      |object a    extends   App {
                      |println("hello world!")
                      |}
                    """.stripMargin
  val expected = """object a extends App {
                   |  println("hello world!")
                   |}
                 """.stripMargin
  test("cli parses args") {
    val expectedStyle =
      ScalafmtConfig.default.copy(maxColumn = 99,
                                  continuationIndentCallSite = 2,
                                  continuationIndentDefnSite = 3,
                                  scalaDocs = false,
                                  alignStripMarginStrings = false)
    val expected = Cli.Config.default.copy(
        style = expectedStyle, file = Some(new File("foo")), inPlace = true)
    val args = Array(
        "--maxColumn",
        "99",
        "--continuationIndentCallSite",
        "2",
        "--continuationIndentDefnSite",
        "3",
        "--javaDocs",
        "--alignStripMarginStrings",
        "false",
        "--file",
        "foo",
        "-i"
    )
    val obtained = Cli.getConfig(args)
    assert(obtained.contains(expected))
  }

  test("scalafmt -i --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      Cli.Config.default.copy(file = Some(tmpFile.toFile), inPlace = true)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, expected)
  }

  test("scalafmt -i ignores non-scala files") {
    val tmpFile = Files.createTempFile("prefix", "suffix")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      Cli.Config.default.copy(file = Some(tmpFile.toFile), inPlace = true)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, unformatted)
  }
}
