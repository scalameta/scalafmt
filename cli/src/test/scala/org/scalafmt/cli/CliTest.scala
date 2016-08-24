package org.scalafmt.cli

import java.io.File
import java.nio.file.Files

import org.scalafmt.AlignToken
import org.scalafmt.Error.MisformattedFile
import org.scalafmt.ScalafmtOptimizer
import org.scalafmt.ScalafmtRunner
import org.scalafmt.ScalafmtStyle
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite

class CliTest extends FunSuite with DiffAssertions {
  import org.scalafmt.util.LoggerOps._
  val unformatted = """
                      |object a    extends   App {
                      |pr(
                      |
                      |"h")
                      |}
                    """.stripMargin
  // Using maxColumn 10 just to see the CLI uses the custom style.
  val expected = """object a
                   |    extends App {
                   |  pr(
                   |    "h")
                   |}
                 """.stripMargin
  val expectedStyle = ScalafmtStyle.default.copy(
    rewriteTokens = Map(
      "=>" -> "⇒",
      "<-" -> "←"
    ),
    indentOperatorsIncludeFilter = ScalafmtStyle.indentOperatorsIncludeAkka,
    indentOperatorsExcludeFilter = ScalafmtStyle.indentOperatorsExcludeAkka,
    reformatDocstrings = false,
    maxColumn = 99,
    alignMixedOwners = true,
    unindentTopLevelOperators = true,
    continuationIndentCallSite = 2,
    continuationIndentDefnSite = 3,
    scalaDocs = false,
    binPackImportSelectors = false,
    poorMansTrailingCommasInConfigStyle = true,
    alignStripMarginStrings = false,
    spaceBeforeContextBoundColon = true)
  val expectedConfig = Cli.Config.default.copy(
    debug = true,
    runner = ScalafmtRunner.statement.copy(
      optimizer = ScalafmtOptimizer.default.copy(bestEffortEscape = true)),
    style = expectedStyle,
    files = Seq(new File("foo")),
    inPlace = true)
  val args = Array(
    "--poorMansTrailingCommasInConfigStyle",
    "true",
    "--reformatComments",
    "false",
    "--binPackImportSelectors",
    "false",
    "--unindentTopLevelOperators",
    "true",
    "--alignMixedOwners",
    "true",
    "--indentOperators",
    "false",
    "--rewriteTokens",
    "=>;⇒,<-;←",
    "--statement",
    "--bestEffortInDeeplyNestedCode",
    "--debug",
    "--maxColumn",
    "99",
    "--spaceBeforeContextBoundColon",
    "true",
    "--continuationIndentCallSite",
    "2",
    "--continuationIndentDefnSite",
    "3",
    "--javaDocs",
    "--assumeStandardLibraryStripMargin",
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
    val obtained =
      Cli.scoptParser.parse(Seq("--style", "Scala.js"), Cli.Config.default)
    assert(obtained.get.style == ScalafmtStyle.scalaJs)
  }

  test("handles .scala and .sbt files") {
    val dir = File.createTempFile("dir", "dir")
    dir.delete()
    dir.mkdir()
    val file1 = File.createTempFile("foo", ".scala", dir)
    val file2 = File.createTempFile("foo", ".sbt", dir)
    val original1 = """
        |object   a {
        |println(1)
        |}
      """.stripMargin
    val expected1 = """
        |object a {
        |  println(1)
        |}
      """.stripMargin
    val original2 = """
        |lazy val x = project
        |.dependsOn(core)
        |
        |lazy val y =    project.dependsOn(core)
      """.stripMargin
    val expected2 = """
        |lazy val x = project.dependsOn(core)
        |
        |lazy val y = project.dependsOn(core)
      """.stripMargin
    FileOps.writeFile(file1.getAbsolutePath, original1)
    FileOps.writeFile(file2.getAbsolutePath, original2)
    val config = Cli.Config.default.copy(inPlace = true, files = Seq(dir))
    Cli.run(config)
    val obtained1 = FileOps.readFile(file1)
    val obtained2 = FileOps.readFile(file2)
    assertNoDiff(obtained2, expected2)
  }
}
