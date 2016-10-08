package org.scalafmt.cli

import java.io.File
import java.nio.file.Files

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite

class CliTest extends FunSuite with DiffAssertions {
  import FileTestOps._
  val unformatted = """
                      |object a    extends   App {
                      |pr(
                      |
                      |"h")
                      |}
                    """.stripMargin
  // Using maxColumn 10 just to see the CLI uses the custom style.
  val expected10 = """|object a
                      |    extends App {
                      |  pr(
                      |    "h")
                      |}""".stripMargin
  val formatted = """|object a extends App {
                     |  pr("h")
                     |}""".stripMargin
  val sbtOriginal =
    """|lazy val x = project
       |   lazy val y    = project
       |   """.stripMargin

  val sbtExpected =
    """|lazy val x = project
       |lazy val y = project""".stripMargin

  def gimmeConfig(string: String): ScalafmtConfig =
    Config.fromHocon(string) match {
      case Right(e) => e
      case Left(e) => throw e
    }

  test("scalafmt -i --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val args = Array(
      "--config",
      "\"maxColumn=7\"",
      "--in-place",
      "--files",
      tmpFile.toFile.getPath
    )
    val formatInPlace = Cli.getConfig(args).get
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, expected10)
  }

  test("scalafmt --test --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val args = Array(
      "--files",
      tmpFile.toFile.getPath,
      "--test"
    )
    val formatInPlace = Cli.getConfig(args).get
    intercept[MisformattedFile] {
      Cli.run(formatInPlace)
    }
  }

  test("scalafmt -i ignores non-scala files") {
    val tmpFile = Files.createTempFile("prefix", "suffix")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace =
      CliOptions.default
        .copy(inPlace = true)
        .withFiles(Seq(tmpFile.toFile))
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, unformatted)
  }

  test("handles .scala and .sbt files") {
    val input = string2dir(
      s"""|/foobar.scala
          |object    A {  }
          |/foo.sbt
          |lazy   val x   = project
          |""".stripMargin
    )
    val expected =
      s"""|/foo.sbt
          |lazy val x = project
          |
          |/foobar.scala
          |object A {}
          |""".stripMargin
    val options = Cli
      .getConfig(
        Array(
          "--files",
          input.getAbsolutePath,
          "-i"
        ))
      .get
    Cli.run(options)
    val obtained = dir2string(input)
    assertNoDiff(obtained, expected)
  }

  test("excludefilters are respected") {
    val input = string2dir(
      s"""|/foo.sbt
          |lazy   val x   = project
          |/target/generated.scala
          |object    AAAAAAIgnoreME   {  }
          |""".stripMargin
    )
    val expected =
      s"""|/foo.sbt
          |lazy val x = project
          |
          |/target/generated.scala
          |object    AAAAAAIgnoreME   {  }
          |""".stripMargin
    val options = Cli
      .getConfig(
        Array(
          "--files",
          input.getAbsolutePath,
          "--exclude",
          "target",
          "-i"
        ))
      .get
    Cli.run(options)
    val obtained = dir2string(input)
    assertNoDiff(obtained, expected)
  }
}
