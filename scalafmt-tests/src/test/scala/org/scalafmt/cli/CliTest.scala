package org.scalafmt.cli

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import org.scalafmt.Error.NoMatchingFiles
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.OsSpecific._
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite
import FileTestOps._
import java.io.IOException

abstract class AbstractCliTest extends FunSuite with DiffAssertions {

  val ScalafmtVersion = "1.6.0-RC4"

  def mkArgs(str: String): Array[String] =
    str.split(' ')

  def runWith(root: AbsoluteFile, argStr: String): Unit = {
    val args = mkArgs(argStr)
    val opts = getMockOptions(root)

    val conf = Cli.getConfig(args, opts)
    Cli.run(conf.get)
  }

  def getMockOptions(baseDir: AbsoluteFile): CliOptions =
    getMockOptions(baseDir, baseDir)

  def getMockOptions(
      baseDir: AbsoluteFile,
      workingDir: AbsoluteFile,
      out: PrintStream = System.out
  ): CliOptions = {
    CliOptions.default.copy(
      gitOpsConstructor = _ => new FakeGitOps(baseDir),
      common = CliOptions.default.common.copy(
        workingDirectory = workingDir,
        out = out,
        err = out
      )
    )
  }

  val baseCliOptions: CliOptions = getMockOptions(
    AbsoluteFile
      .fromPath(Files.createTempDirectory("base-dir").toString)
      .get)

  def getConfig(args: Array[String]): CliOptions = {
    Cli.getConfig(args, baseCliOptions).get
  }

  val commonScalafmtConfig: Path = Files.createTempFile(".scalafmt", ".conf")
  val commonConfig: String = s"""
                                |version=$ScalafmtVersion
               """.stripMargin
  Files.write(commonScalafmtConfig, commonConfig.getBytes)

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
    Config.fromHoconString(string).get
  def noArgTest(
      input: AbsoluteFile,
      expected: String,
      cmds: Seq[Array[String]],
      assertExit: ExitCode => Unit = { exit =>
        assert(exit.isOk, exit)
      },
      assertOut: String => Unit = { out =>
        println(out)
      }
  ): Unit = {
    cmds.foreach { args =>
      val out = new ByteArrayOutputStream()
      val init: CliOptions = getMockOptions(input, input, new PrintStream(out))
      val config = Cli.getConfig(args, init).get
      val exit = Cli.run(config)
      assertExit(exit)
      val obtained = dir2string(input)
      assertNoDiff(obtained, expected)
      val configTest = Cli.getConfig(Array("--test"), init).get
      Cli.run(configTest)
      assertOut(out.toString())
    }

  }

}

class CliTest extends AbstractCliTest {
  test("scalafmt tmpFile tmpFile2") {
    val originalTmpFile = Files.createTempFile("prefix", ".scala")
    val originalTmpFile2 = Files.createTempFile("prefix2", ".scala")
    val scalafmtConfig = Files.createTempFile("scalafmtConfig", ".scala")
    val config = s"""
                    |version=$ScalafmtVersion
                    |maxColumn=7,
                    |style=IntelliJ
    """.stripMargin
    Files.write(originalTmpFile, unformatted.getBytes)
    Files.write(originalTmpFile2, unformatted.getBytes)
    Files.write(scalafmtConfig, config.getBytes)
    val args = Array(
      "--config",
      scalafmtConfig.toFile.getPath,
      originalTmpFile.toFile.getPath,
      originalTmpFile2.toFile.getPath
    )
    val formatInPlace = getConfig(args)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(originalTmpFile.toString)
    val obtained2 = FileOps.readFile(originalTmpFile2.toString)
    assertNoDiff(obtained, expected10)
    assertNoDiff(obtained2, expected10)
  }

  test("scalafmt --stdout tmpFile prints to stdout") {
    val originalTmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(originalTmpFile, unformatted.getBytes)
    val args = Array(
      "--stdout",
      "--config",
      commonScalafmtConfig.toFile.getPath,
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
    val scalafmtConfig = Files.createTempFile(".scalafmt", ".conf")
    val config = s"""
                    |version=$ScalafmtVersion
                    |maxColumn=7
                    |style=IntelliJ
    """.stripMargin
    Files.write(scalafmtConfig, config.getBytes)

    val args = Array(
      "--stdin",
      "--assume-filename",
      "build.sbt",
      "--config",
      scalafmtConfig.toFile.getPath
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

  test("scalafmt --test tmpFile is left unformmated") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val args = Array(
      tmpFile.toFile.getPath,
      "--test",
      "--config",
      commonScalafmtConfig.toFile.getPath
    )
    val formatInPlace = getConfig(args)
    val exit = Cli.run(formatInPlace)
    assert(exit.is(ExitCode.TestError))
    val str = FileOps.readFile(tmpFile.toString)
    assertNoDiff(str, unformatted)
  }

  test("scalafmt foo.randomsuffix is formatted") {
    val tmpFile = Files.createTempFile("prefix", "randomsuffix")
    Files.write(tmpFile, unformatted.getBytes)
    val args = Array(
      "--config",
      commonScalafmtConfig.toFile.getPath,
      tmpFile.toFile.getAbsolutePath
    )
    Cli.exceptionThrowingMain(args)
    val obtained = FileOps.readFile(tmpFile.toString)
    // TODO: We need to pass customFiles information to ProjectFiles
    assertNoDiff(obtained, formatted)
  }

  test("handles .scala, .sbt, and .sc files") {
    val input = string2dir(
      s"""|/foobar.scala
          |object    A {  }
          |/foo.sbt
          |lazy   val x   = project
          |/foo.sc
          |lazy   val x   = project
          |""".stripMargin
    )
    val expected =
      s"""|/foo.sbt
          |lazy val x = project
          |
          |/foo.sc
          |lazy val x = project
          |
          |/foobar.scala
          |object A {}
          |""".stripMargin
    val options = getConfig(
      Array(
        input.path,
        "--config",
        commonScalafmtConfig.toFile.getPath
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
        "--config",
        commonScalafmtConfig.toFile.getPath,
        input.path,
        "--exclude",
        "target/nested".asFilename
      ))
    Cli.run(options)
    val obtained = dir2string(input)
    assertNoDiff(obtained, expected)
  }

  test("scalafmt doesnotexist.scala throws error") {
    def check(filename: String): Unit = {
      val args = Array(
        s"$filename.scala".asFilename,
        "--config",
        commonScalafmtConfig.toFile.getPath
      )
      intercept[IOException] {
        Cli.exceptionThrowingMain(args)
      }
    }
    check("notfound")
    check("target/notfound")
  }

  test("scalafmt (no matching files) throws error") {
    val options = baseCliOptions.copy(config = Some(commonScalafmtConfig))
    intercept[NoMatchingFiles.type] {
      Cli.run(options)
    }
  }

  test("scalafmt (no matching files) is okay with --diff and --stdin") {
    val diff = getConfig(
      Array("--diff", "--config", commonScalafmtConfig.toFile.getPath))
    val stdin = getConfig(
      Array("--stdin", "--config", commonScalafmtConfig.toFile.getPath)).copy(
      common = CommonOptions(in = new ByteArrayInputStream("".getBytes))
    )
    Cli.run(diff)
    Cli.run(stdin)
  }

  test("scalafmt (no arg) read config from git repo") {
    val input = string2dir(
      s"""|/foo.scala
          |object    FormatMe {
          |  val x = 1
          |}
          |/target/foo.scala
          |object A   { }
          |
          |/.scalafmt.conf
          |version = $ScalafmtVersion
          |maxColumn = 2
          |project.excludeFilters = [target]
          |""".stripMargin
    )

    val expected =
      s"""|/.scalafmt.conf
          |version = $ScalafmtVersion
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
           |/foo.sc
           |object    FormatMe
           |""".stripMargin
      ),
      """|/foo.sc
         |object FormatMe
         |
         |/foo.scala
         |object FormatMe
         |""".stripMargin,
      Seq(
        Array("--config", commonScalafmtConfig.toFile.getPath)
      )
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
          |version=$ScalafmtVersion
          |maxColumn = 2
          |""".stripMargin
    )
    val workingDir = input / "nested"
    val options: CliOptions = {
      val mock = getMockOptions(input, workingDir)
      mock.copy(common = mock.common.copy(workingDirectory = workingDir))
    }
    val config = Cli.getConfig(Array("foo.scala"), options).get
    Cli.run(config)
    val obtained = FileOps.readFile(workingDir / "foo.scala")
    assertNoDiff(obtained, expected)
  }

  test(
    "if project.includeFilters isn't modified (and files aren't passed manually), it should ONLY accept scala and sbt files") {

    val root =
      string2dir(
        s"""
           |/scalafmt.conf
           |style = default
           |version = $ScalafmtVersion
           |/scalafile.scala
           |$unformatted
           |/scalatex.scalatex
           |$unformatted
           |/sbt.sbt
           |$sbtOriginal
           |/sbt.sbtfile
           |$sbtOriginal""".stripMargin
      )

    val config = root / "scalafmt.conf"
    val args = mkArgs(s"--config $config")
    val opts = getMockOptions(root)

    val conf = Cli.getConfig(args, opts)
    Cli.run(conf.get)

    assertNoDiff(root / "scalatex.scalatex", unformatted)
    assertNoDiff(root / "sbt.sbtfile", sbtOriginal)

    assertNoDiff(root / "scalafile.scala", formatted)
    val sbtFormatted =
      """|lazy val x = project
         |lazy val y = project
         |""".stripMargin
    assertNoDiff(root / "sbt.sbt", sbtFormatted)
  }

  test(
    "includeFilters are ignored for full paths but NOT ignore for passed directories") {

    val root =
      string2dir(
        s"""
           |/inner/file1.scala
           |$unformatted
           |/inner2/file2.scalahala
           |$unformatted
           |/inner2/file3.scalahala
           |$unformatted""".stripMargin
      )
    val inner1 = root / "inner"
    val inner2 = root / "inner2"
    val full = inner2 / "file3.scalahala"

    runWith(
      root,
      s"--config ${commonScalafmtConfig.toFile.getPath} $inner1 $inner2 $full")

    assertNoDiff(inner1 / "file1.scala", formatted)
    assertNoDiff(inner2 / "file2.scalahala", unformatted)
    assertNoDiff(full, formatted)
  }

  test("--config accepts absolute paths") {
    val root = string2dir(
      s"""/scalafmt.conf
         |version = $ScalafmtVersion
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
      toFormat
    )
    Cli.exceptionThrowingMain(args) // runs without errors
    val obtained = FileOps.readFile(toFormat)
    assertNoDiff(obtained, "object A\n")
  }

  // These are tests for deprecated flags
  test("scalafmt -i -f file1,file2,file3 should still work") {

    val file1 = Files.createTempFile("prefix", ".scala")
    val file2 = Files.createTempFile("prefix2", ".scala")
    val file3 = Files.createTempFile("prefix3", ".scala")
    Files.write(file1, unformatted.getBytes)
    Files.write(file2, unformatted.getBytes)
    Files.write(file3, unformatted.getBytes)
    def fileStr(fs: Path*) = fs.map(_.toFile.getPath).mkString(",")
    val args = Array(
      "--config",
      commonScalafmtConfig.toFile.getPath,
      "-i",
      "-f",
      fileStr(file1, file2, file3)
    )
    val formatInPlace = getConfig(args)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(file1.toString)
    val obtained2 = FileOps.readFile(file2.toString)
    val obtained3 = FileOps.readFile(file3.toString)
    assertNoDiff(obtained, formatted)
    assertNoDiff(obtained2, formatted)
    assertNoDiff(obtained3, formatted)
  }

  test("parse error is formatted nicely") {
    val input =
      """|/foo.scala
         |object    A { foo( }
         |""".stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(Array("--config", commonScalafmtConfig.toFile.getPath)),
      assertExit = { exit =>
        assert(exit.is(ExitCode.ParseError))
      },
      assertOut = out => {
        assert(
          out.contains(
            """foo.scala:1: error: illegal start of simple expression
              |object    A { foo( }
              |                   ^""".stripMargin
          )
        )
      }
    )
  }

  test("command line argument error") {
    val exit = Cli.mainWithOptions(
      Array("--foobar"),
      getMockOptions(AbsoluteFile.userDir)
    )
    assert(exit.is(ExitCode.CommandLineArgumentError), exit)
  }

  test("--test failure prints out unified diff") {
    val input =
      s"""|/.scalafmt.conf
          |onTestFailure = "To fix this ..."
          |version = $ScalafmtVersion
          |
          |/foo.scala
          |object    A { }
          |""".stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(Array("--test")),
      assertExit = { exit =>
        assert(exit.is(ExitCode.TestError))
      },
      assertOut = out => {
        assert(
          out.contains(
            """|foo.scala-formatted
               |@@ -1,1 +1,1 @@
               |-object    A { }
               |+object A {}
               |error: --test failed
               |To fix this ...""".stripMargin
          )
        )
      }
    )
  }

  test("--test succeeds even with parse error") {
    val input =
      """|/foo.scala
         |object A {
         |""".stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(Array("--test", "--config", commonScalafmtConfig.toFile.getPath)),
      assertExit = { exit =>
        assert(exit.isOk)
      },
      assertOut = out => {
        println(s"succeed: $out")
        assert(
          out.contains(
            "foo.scala:2: error: } expected but end of file found"
          ) &&
            out.contains(
              "error: ParseError=2"
            )
        )
      }
    )
  }

  test("--test fails with parse error if fatalWarnings=true") {
    val input =
      s"""|/.scalafmt.conf
          |runner.fatalWarnings = true
          |version = $ScalafmtVersion
          |/foo.scala
          |object A {
          |""".stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(Array("--test")),
      assertExit = { exit =>
        assert(exit == ExitCode.ParseError)
      },
      assertOut = out => {
        assert(
          out.contains(
            "foo.scala:2: error: } expected but end of file found"
          ) && out.contains(
            "error: ParseError=2"
          )
        )
      }
    )
  }

  test("exception is thrown on invalid .scalafmt.conf") {
    val input =
      s"""/.scalafmt.conf
         |version=$ScalafmtVersion
         |blah = intellij
         |/foo.scala
         |object A {}
      """.stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(Array.empty),
      assertExit = { exit =>
        assert(exit == ExitCode.UnexpectedError)
      },
      assertOut = out => {
        assert(
          out.contains(
            "Invalid field: blah"
          )
        )
      }
    )
  }

  test("eof") {
    val in = Files.createTempFile("scalafmt", "Foo.scala")
    Files.write(in, "object A".getBytes(StandardCharsets.UTF_8))
    val exit = Cli.mainWithOptions(Array(in.toString), CliOptions.default)
    assert(exit.isOk)
    val obtained = new String(Files.readAllBytes(in), StandardCharsets.UTF_8)
    assert(obtained == "object A\n")
  }
}
