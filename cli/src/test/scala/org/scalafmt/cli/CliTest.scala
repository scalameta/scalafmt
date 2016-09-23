package org.scalafmt.cli

import java.io.File
import java.nio.file.Files

import org.scalafmt.LineEndings._
import org.scalafmt.Error.MisformattedFile
import org.scalafmt.ScalafmtStyle
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite

class CliTest extends FunSuite with DiffAssertions {
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
    "--keepSelectChainLineBreaks",
    "false",
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

  test("reads .scalafmt.conf") {
    val expectedStyle = ScalafmtStyle.default40
    val tmpFile = Files.createTempFile("prefix", ".scalafmt.conf")
    Files.write(tmpFile, "style=40".getBytes)
    val args = Array("--config", ".scalafmt.conf")

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

  test("ignores files if told so by the configuration") {
    val dir = File.createTempFile("dir", "dir")
    dir.delete()
    dir.mkdir()
    val file1 = File.createTempFile("foo", ".scala", dir)
    val file2 = File.createTempFile("bar", ".scala", dir)
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
                      |object   a {
                      |println(1)
                      |}
                    """.stripMargin
    val expected2 = """
                      |object   a {
                      |println(1)
                      |}
                    """.stripMargin
    FileOps.writeFile(file1.getAbsolutePath, original1)
    FileOps.writeFile(file2.getAbsolutePath, original2)
    val config = Cli.Config.default
      .copy(inPlace = true, files = Seq(dir), exclude = Seq(file2))
    Cli.run(config)
    val obtained1 = FileOps.readFile(file1)
    val obtained2 = FileOps.readFile(file2)
    assertNoDiff(obtained1, expected1)
    assertNoDiff(obtained2, expected2)
  }

  test("migrate") {
    val result = Cli.migrate(
      """
        |--maxColumn 100 # comment
        |--alignTokens %;Infix,%%;Infix
        |--alignTokens "a;b,c;d,e;.*" # comment
        |--reformatComments false
        |--scalaDocs false
        |--scalaDocs true
        |--alignStripMarginStrings true
        |--binPackArguments true
        |--binPackParameters true
        |--binPackParentConstructors true
        |--configStyleArguments false
        |--noNewlinesBeforeJsNative false
        |--alignByOpenParenCallSite false
        |--alignByOpenParenDefnSite false
        |--continuationIndentCallSite 3
        |--continuationIndentDefnSite 3
        |--alignMixedOwners false
        |--binPackImportSelectors true
        |--spacesInImportCurlyBraces true
        |--spaceAfterTripleEquals true
        |--spaceBeforeContextBoundColon true
        |--unindentTopLevelOperators false
        |--bestEffortInDeeplyNestedCode
      """.stripMargin
    )
    val expected =
      """
        |maxColumn = 100 # comment
        |align.tokens = [
        |  { code = "%", owner = "Infix" }
        |  { code = "%%", owner = "Infix" }
        |]
        |align.tokens = [ # comment
        |  { code = "a", owner = "b" }
        |  { code = "c", owner = "d" }
        |  "e"
        |]
        |docstrings = preserve
        |docstrings = JavaDoc
        |docstrings = ScalaDoc
        |assumeStandardLibraryStripMargin = true
        |binPack.callSite = true
        |binPack.defnSite = true
        |binPack.parentConstructors = true
        |configStyleArguments = false
        |noNewlinesBeforeJsNative = false
        |align.openParenCallSite = false
        |align.openParenDefnSite = false
        |continuationIndent.callSite = 3
        |continuationIndent.defnSite = 3
        |align.mixedOwners = false
        |binPackImportSelectors = true
        |spaces.inImportCurlyBraces = true
        |spaces.afterTripleEquals = true
        |spaces.beforeContextBoundColon = true
        |unindentTopLevelOperators = false
        |bestEffortInDeeplyNestedCode = true
      """.stripMargin
    println(result)
    assertNoDiff(result, expected)
  }

  test("--config can be string") {
    val Right(obtained) = Cli.getConfig(
      Array(
        "--config",
        """"maxColumn=10""""
      ))
    assert(obtained.style.maxColumn == 10)
  }

}
