package org.scalafmt.cli

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileNotFoundException
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite

class CliTest extends FunSuite with DiffAssertions {
  import FileTestOps._

  def getMockOptions(baseDir: AbsoluteFile): CliOptions =
    getMockOptions(baseDir, baseDir)

  def getMockOptions(baseDir: AbsoluteFile,
                     workingDir: AbsoluteFile): CliOptions = {
    CliOptions.default.copy(
      gitOpsConstructor = x => new FakeGitOps(baseDir),
      common = CliOptions.default.common.copy(
        workingDirectory = workingDir
      )
    )
  }

  val baseCliOptions: CliOptions = getMockOptions(
    AbsoluteFile
      .fromPath(File.createTempFile("base", "dir").getAbsolutePath)
      .get)

  def getConfig(args: Array[String]): CliOptions = {
    Cli.getConfig(args, baseCliOptions).get
  }
  val unformatted = """
                      |object a    extends   App {
                      |pr("h")
                      |}
                    """.stripMargin
  // Using maxColumn 10 just to see the CLI uses the custom style.
  val expected10 = """|object a
                      |    extends App {
                      |  pr(
                      |    "h"
                      |  )
                      |}
                      |""".stripMargin
  val formatted = """|object a extends App {
                     |  pr("h")
                     |}
                     |""".stripMargin
  val customConfig =
    """
      |maxColumn   = 2
    """.stripMargin
  val sbtOriginal =
    """|lazy val x = project
       |   lazy val y    = project
       |   """.stripMargin

  val sbtExpected =
    """|lazy val x =
       |  project
       |lazy val y =
       |  project
       |""".stripMargin

  def gimmeConfig(string: String): ScalafmtConfig =
    Config.fromHocon(string).get

  test("scalafmt -i --file tmpFile") {
    val originalTmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(originalTmpFile, unformatted.getBytes)
    val args = Array(
      "--config-str",
      "{maxColumn=7,style=IntelliJ}",
      "--in-place",
      "--files",
      originalTmpFile.toFile.getPath
    )
    val formatInPlace = getConfig(args)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(originalTmpFile.toString)
    assertNoDiff(obtained, expected10)
  }

  test("scalafmt --file tmpFile prints to stdout") {
    val originalTmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(originalTmpFile, unformatted.getBytes)
    val args = Array(
      "-f",
      originalTmpFile.toFile.getPath
    )
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val init = baseCliOptions.copy(
      common = baseCliOptions.common.copy(out = ps)
    )
    val auto = Cli.getConfig(args, init).get
    Cli.run(auto)
    val obtained = new String(baos.toByteArray, StandardCharsets.UTF_8)
    assertNoDiff(obtained, formatted)
    assert(obtained.size == formatted.size)
  }

  test("scalafmt --stdin --assume-filename") {
    val args = Array(
      "--stdin",
      "--assume-filename",
      "build.sbt",
      "--config-str",
      "{maxColumn=7,style=IntelliJ}"
    )
    val printToStdout = getConfig(args)
    val bais = new ByteArrayInputStream(sbtOriginal.getBytes)
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    Cli.run(
      printToStdout.copy(
        common = printToStdout.common.copy(
          out = ps,
          in = bais
        )
      ))
    val obtained = new String(baos.toByteArray, StandardCharsets.UTF_8)
    assertNoDiff(obtained, sbtExpected)
    assert(obtained.size == sbtExpected.size)
  }

  test("scalafmt --test --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val args = Array(
      "--files",
      tmpFile.toFile.getPath,
      "--test"
    )
    val formatInPlace = getConfig(args)
    intercept[MisformattedFile] {
      Cli.run(formatInPlace)
    }
  }

  test("scalafmt -i -f foo.randomsuffix is formatted") {
    val tmpFile = Files.createTempFile("prefix", "randomsuffix")
    Files.write(tmpFile, unformatted.getBytes)
    val args = Array(
      "-i",
      "-f",
      tmpFile.toFile.getAbsolutePath
    )
    Cli.main(args)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, formatted)
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
    val options = getConfig(
      Array(
        "--files",
        input.path,
        "-i"
      )
    )
    Cli.run(options)
    val obtained = dir2string(input)
    assertNoDiff(obtained, expected)
  }

  test("excludefilters are respected") {
    val input = string2dir(
      s"""|/foo.sbt
          |lazy   val x   = project
          |
          |/target/FormatMe.scala
          |object    PleaseFormatMeOtherwiseIWillBeReallySad   {  }
          |
          |/target/nested/DoNotFormatMe.scala
          |object    AAAAAAIgnoreME   {  }
          |
          |/target/nested/nested2/DoNotFormatMeToo.scala
          |object    BBBBBBIgnoreME   {  }
          |""".stripMargin
    )
    val expected =
      s"""|/foo.sbt
          |lazy val x = project
          |
          |/target/FormatMe.scala
          |object PleaseFormatMeOtherwiseIWillBeReallySad {}
          |
          |/target/nested/DoNotFormatMe.scala
          |object    AAAAAAIgnoreME   {  }
          |
          |/target/nested/nested2/DoNotFormatMeToo.scala
          |object    BBBBBBIgnoreME   {  }
          |""".stripMargin
    val options = getConfig(
      Array(
        "--files",
        input.path,
        "--exclude",
        "target/nested",
        "-i"
      ))
    Cli.run(options)
    val obtained = dir2string(input)
    assertNoDiff(obtained, expected)
  }

  test("--file doesnotexist.scala throws error") {
    def check(filename: String): Unit = {
      val args = Array("-f", s"$filename.scala")
      intercept[FileNotFoundException] {
        Cli.main(args)
      }
    }
    check("notfound")
    check("target/notfound")
  }

  def noArgTest(input: AbsoluteFile,
                expected: String,
                cmds: Seq[Array[String]]): Unit = {
    cmds.foreach { args =>
      val init: CliOptions = getMockOptions(input)
      val config = Cli.getConfig(args, init).get
      Cli.run(config)
      val obtained = dir2string(input)
      assertNoDiff(obtained, expected)
      val configTest = Cli.getConfig(Array("--test"), init).get
      Cli.run(configTest)
    }

  }

  test("scalafmt (no arg) read config from git repo") {
    val input = string2dir(
      """|/foo.scala
         |object    FormatMe {
         |  val x = 1
         |}
         |/target/foo.scala
         |object A   { }
         |/.scalafmt.conf
         |maxColumn = 2
         |project.excludeFilters = [target]
         |""".stripMargin
    )

    val expected =
      """|/.scalafmt.conf
         |maxColumn = 2
         |project.excludeFilters = [target]
         |
         |/foo.scala
         |object FormatMe {
         |  val x =
         |    1
         |}
         |
         |/target/foo.scala
         |object A   { }
         |""".stripMargin
    noArgTest(
      input,
      expected,
      Seq(Array.empty[String], Array("--diff"))
    )
  }
  test("scalafmt (no arg, no config)") {
    noArgTest(
      string2dir(
        """|/foo.scala
           |object    FormatMe
           |""".stripMargin
      ),
      """|/foo.scala
         |object FormatMe
         |""".stripMargin,
      Seq(Array.empty[String])
    )
  }

  test("config is read even from nested dir") {
    val original = "object a { val x = 1 }"
    val expected =
      """|object a {
         |  val x =
         |    1
         |}
         |""".stripMargin
    val input = string2dir(
      s"""|/nested/foo.scala
          |$original
          |/.scalafmt.conf
          |maxColumn = 2
          |""".stripMargin
    )
    val workingDir = input / "nested"
    val options: CliOptions = {
      val mock = getMockOptions(input, workingDir)
      mock.copy(common = mock.common.copy(workingDirectory = workingDir))
    }
    val config = Cli.getConfig(Array("-i", "-f", "foo.scala"), options).get
    Cli.run(config)
    val obtained = FileOps.readFile(workingDir / "foo.scala")
    assertNoDiff(obtained, expected)
  }

  test("--config accepts absolute paths") {
    val root = string2dir(
      """/scalafmt.conf
        |style = intellij
        |/foo.scala
        |object    A
      """.stripMargin
    )
    val config = (root / "scalafmt.conf").path
    val toFormat = (root / "foo.scala").path
    val args = Array[String](
      "--config",
      config,
      "-i",
      "-f",
      toFormat
    )
    Cli.main(args) // runs without errors
    val obtained = FileOps.readFile(toFormat)
    assertNoDiff(obtained, "object A\n")
  }
}
