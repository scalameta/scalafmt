package org.scalafmt.cli

import java.io.File
import java.nio.file.Files

import org.scalafmt.AlignToken
import org.scalafmt.Error.MisformattedFile
import org.scalafmt.ScalafmtStyle
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite
import shapeless.ops.hlist.Align

class CliTest extends FunSuite with DiffAssertions {
  import org.scalafmt.util.LoggerOps._
  val unformatted = """
                      |object a    extends   App {
                      |println("hello world!")
                      |}
                    """.stripMargin
  // Using maxColumn 10 just to see the CLI uses the custom style.
  val expected = """object a
                   |    extends App {
                   |  println(
                   |      "hello world!")
                   |}
                 """.stripMargin
  val expectedStyle = ScalafmtStyle.default.copy(
      maxColumn = 99,
      continuationIndentCallSite = 2,
      continuationIndentDefnSite = 3,
      scalaDocs = false,
      alignStripMarginStrings = false)
  val expectedConfig = Cli.Config.default
    .copy(style = expectedStyle, files = Seq(new File("foo")), inPlace = true)
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
      "--files",
      "foo",
      "-i"
  )

  test("cli parses args") {
    val obtained = Cli.getConfig(args)
    assert(obtained.contains(expectedConfig))
  }

  test("cli parses style from config file") {
    val tmpFile = Files.createTempFile("prefix", ".scalafmt")
    val contents = s"""
                      |# Config files without comments suck.
                      |${args.mkString("\n").replaceFirst("\n", " ")}
                      |--alignTokens #;Template,//;.*
      """.stripMargin
    Files.write(tmpFile, contents.getBytes)
    val externalConfigArgs = Array("--config", tmpFile.toAbsolutePath.toString)
    val expectedCustomStyle = expectedStyle.copy(
        alignTokens = Set(AlignToken("#", "Template"), AlignToken("//", ".*")))
    val obtained = Cli.getConfig(externalConfigArgs)
    assert(obtained.exists(_.style == expectedCustomStyle))
  }

  test("scalafmt -i --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace = Cli.Config.default.copy(
        style = ScalafmtStyle.default.copy(maxColumn = 7),
        files = Seq(tmpFile.toFile),
        inPlace = true)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, expected)
  }

  test("scalafmt --test --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      Cli.Config.default.copy(files = Seq(tmpFile.toFile), testing = true)
    intercept[MisformattedFile] {
      Cli.run(formatInPlace)
    }
  }

  test("scalafmt -i ignores non-scala files") {
    val tmpFile = Files.createTempFile("prefix", "suffix")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      Cli.Config.default.copy(files = Seq(tmpFile.toFile), inPlace = true)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, unformatted)
  }
  test("--style Scala.js is OK") {
    val obtained = Cli.parser.parse(Seq("--style", "Scala.js"), Cli.Config.default)
    assert(obtained.get.style == ScalafmtStyle.scalaJs)
  }
}
