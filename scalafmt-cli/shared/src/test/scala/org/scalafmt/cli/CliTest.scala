package org.scalafmt.cli

import org.scalafmt.Error.NoMatchingFiles
import org.scalafmt.Versions.{stable => stableVersion}
import org.scalafmt.cli.FileTestOps._
import org.scalafmt.config.{ProjectFiles, ScalafmtConfig}
import org.scalafmt.sysops.OsSpecific._
import org.scalafmt.sysops.PlatformRunOps.executionContext
import org.scalafmt.sysops.{AbsoluteFile, PlatformCompat, PlatformFileOps}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

import munit.FunSuite

abstract class AbstractCliTest extends FunSuite {

  def mkArgs(str: String): Array[String] = str.split(' ')

  def runWith(root: AbsoluteFile, argStr: String)(implicit
      loc: munit.Location,
  ): Future[Unit] = runArgs(getMockOptions(root))(mkArgs(argStr): _*)

  def runArgs(
      cliOptions: CliOptions = baseCliOptions,
      exitCode: ExitCode = ExitCode.Ok,
  )(args: String*)(implicit loc: munit.Location): Future[Unit] =
    run(Cli.getConfig(cliOptions, args: _*).get, exitCode)

  def run(options: CliOptions, exitCode: ExitCode = ExitCode.Ok)(implicit
      loc: munit.Location,
  ): Future[Unit] = Cli.run(options).map(assertEquals(_, exitCode))

  def getConfig(args: String*): CliOptions = Cli
    .getConfig(baseCliOptions, args: _*).get

  def assertContains(out: String, expected: String, flag: Boolean = true)(
      implicit loc: munit.Location,
  ): Unit = assert(
    flag == CliTest.stripCR(out).contains(CliTest.stripCR(expected)),
    s"$out\n should ${if (flag) "" else " NOT "} have contained\n$expected",
  )
  def assertNotContains(out: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = assertContains(out, expected, flag = false)

  val unformatted =
    """|
       |object a    extends   App {
       |pr("h")
       |}
       |                    """.stripMargin
  // Using maxColumn 10 just to see the CLI uses the custom style.
  val expected10 =
    """|object a
       |    extends App {
       |  pr(
       |    "h"
       |  )
       |}
       |""".stripMargin
  val formatted =
    """|object a extends App {
       |  pr("h")
       |}
       |""".stripMargin
  val customConfig =
    """|
       |maxColumn   = 2
       |      """.stripMargin
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

  def gimmeConfig(string: String): ScalafmtConfig = ScalafmtConfig
    .fromHoconString(string).get

  def noArgTest(
      input: AbsoluteFile,
      expected: String,
      cmds: Seq[Array[String]],
      exitCode: ExitCode = ExitCode.Ok,
      assertOut: String => Unit = { _ => },
      testExitCode: Option[ExitCode] = None,
  ): Future[Unit] = {
    def runCmd(args: Array[String]): Future[Unit] = {
      val out = new ByteArrayOutputStream()
      val config = Cli
        .getConfig(getMockOptions(input, input, new PrintStream(out)), args: _*)
        .get
      run(config, exitCode).flatMap { _ =>
        val obtained = dir2string(input)
        assertNoDiff(obtained, expected)
        val testConfig = config.copy(writeModeOpt = None)
        runArgs(testConfig, testExitCode.getOrElse(exitCode))("--test")
          .map(_ => assertOut(out.toString()))
      }
    }
    cmds.foldLeft(Future.successful(())) { case (res, args) =>
      res.map(_ => runCmd(args))
    }
  }

}

trait CliTestBehavior {
  this: AbstractCliTest =>
  def testCli(version: String): Unit = {
    val isCore = version == stableVersion
    val label = if (isCore) "core" else "dynamic"
    val dialectError = if (isCore) " [dialect default]" else ""
    test(s"scalafmt tmpFile tmpFile2: $label") {
      val tmpdir = PlatformFileOps.mkdtemp("clitest")
      val originalTmpFile = tmpdir.resolve("foo1.scala")
      val originalTmpFile2 = tmpdir.resolve("foo2.scala")
      val scalafmtConfig = tmpdir.resolve("scalafmtConfig.scala")
      val config =
        s"""|
            |version="$version"
            |maxColumn=7
            |style=IntelliJ
            |    """.stripMargin
      PlatformFileOps.writeFile(originalTmpFile, unformatted)
      PlatformFileOps.writeFile(originalTmpFile2, unformatted)
      PlatformFileOps.writeFile(scalafmtConfig, config)
      runArgs()(
        "--config",
        scalafmtConfig.toFile.getPath,
        originalTmpFile.toFile.getPath,
        originalTmpFile2.toFile.getPath,
      ).map { _ =>
        val obtained = PlatformFileOps.readFile(originalTmpFile)
        val obtained2 = PlatformFileOps.readFile(originalTmpFile2)
        assertNoDiff(obtained, expected10)
        assertNoDiff(obtained2, expected10)
      }
    }

    test(s"scalafmt --stdout tmpFile prints to stdout: $label") {
      val originalTmpFile = PlatformFileOps.mkdtemp(label)
        .resolve("prefix.scala")
      PlatformFileOps.writeFile(originalTmpFile, unformatted)
      val baos = new ByteArrayOutputStream()
      val ps = new PrintStream(baos)
      runArgs(baseCliOptions.copy(common = baseCliOptions.common.copy(out = ps)))(
        "--stdout",
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
        originalTmpFile.toFile.getPath,
      ).map { _ =>
        val obtained = new String(baos.toByteArray, StandardCharsets.UTF_8)
        assertNoDiff(obtained, formatted)
        assertEquals(obtained.length, formatted.length)
      }
    }

    test(s"scalafmt --stdin --assume-filename: $label") {
      val scalafmtConfig = PlatformFileOps.mkdtemp(label)
        .resolve(".scalafmt.conf")
      val config =
        s"""|
            |version="$version"
            |maxColumn=7
            |style=IntelliJ
            |    """.stripMargin
      PlatformFileOps.writeFile(scalafmtConfig, config)

      val printToStdout = getConfig(
        "--stdin",
        "--assume-filename",
        "build.sbt",
        "--config",
        scalafmtConfig.toFile.getPath,
      )
      val bais = new ByteArrayInputStream(sbtOriginal.getBytes)
      val baos = new ByteArrayOutputStream()
      val ps = new PrintStream(baos)
      val common = printToStdout.common.copy(out = ps, in = bais)
      run(printToStdout.copy(common = common)).map { _ =>
        val obtained = new String(baos.toByteArray, StandardCharsets.UTF_8)
        assertNoDiff(obtained, sbtExpected)
        assertEquals(obtained.length, sbtExpected.length)
      }
    }

    test(s"scalafmt --test tmpFile is left unformatted: $label") {
      val tmpFile = PlatformFileOps.mkdtemp(label).resolve("prefix.scala")
      PlatformFileOps.writeFile(tmpFile, unformatted)
      runArgs(exitCode = ExitCode.TestError)(
        tmpFile.toFile.getPath,
        "--test",
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
      ).map { _ =>
        val str = PlatformFileOps.readFile(tmpFile)
        assertNoDiff(str, unformatted)
      }
    }
    test(s"scalafmt --test fails with non zero exit code $label") {
      val tmpFile = PlatformFileOps.mkdtemp(label).resolve("prefix.scala")
      PlatformFileOps.writeFile(tmpFile, unformatted)
      runArgs(exitCode = ExitCode.UnexpectedError)(
        tmpFile.toFile.getPath,
        "--test",
        "--config-str",
        s"""{version="$version",style=IntelliJ, docstring = Asterisk}""",
      )
    }

    test(s"scalafmt foo.randomsuffix is formatted: $label") {
      val tmpFile = PlatformFileOps.mkdtemp(label).resolve("prefix.randomsuffix")
      PlatformFileOps.writeFile(tmpFile, unformatted)
      val args = Array(
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
        tmpFile.toFile.getAbsolutePath,
      )
      Cli.mainWithOptions(baseCliOptions, args: _*).map { exit =>
        assertEquals(exit, ExitCode.Ok)
        val obtained = PlatformFileOps.readFile(tmpFile)
        // TODO: We need to pass customFiles information to ProjectFiles
        assertNoDiff(obtained, formatted)
      }
    }

    test(s"handles .scala, .sbt, and .sc files: $label") {
      val input = string2dir(
        s"""|/foobar.scala
            |object    A {  }
            |/foo.sbt
            |lazy   val x   = project
            |/foo.sc
            |lazy   val x   = project
            |""".stripMargin,
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
      runArgs()(
        input.toString(),
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
      ).map { _ =>
        val obtained = dir2string(input)
        assertNoDiff(obtained, expected)
      }
    }

    test(s"excludefilters are respected: $label") {
      val input = string2dir {
        s"""|/foo.sbt
            |lazy   val x   = project
            |
            |/target/FormatMe.scala
            |object    PleaseFormatMeOtherwiseIWillBeReallySad   {  }
            |
            |/target/nested1/DoNotFormatMe.scala
            |object    AAAAAAIgnoreME   {  }
            |
            |/target/nested1/nested2/DoNotFormatMeToo.scala
            |object    BBBBBBIgnoreME   {  }
            |
            |/target/nested3/DoNotFormatMe.scala
            |object    CIgnoreME   {  }
            |""".stripMargin
      }
      val expected =
        s"""|/foo.sbt
            |lazy val x = project
            |
            |/target/FormatMe.scala
            |object PleaseFormatMeOtherwiseIWillBeReallySad {}
            |
            |/target/nested1/DoNotFormatMe.scala
            |object    AAAAAAIgnoreME   {  }
            |
            |/target/nested1/nested2/DoNotFormatMeToo.scala
            |object    BBBBBBIgnoreME   {  }
            |
            |/target/nested3/DoNotFormatMe.scala
            |object    CIgnoreME   {  }
            |""".stripMargin
      runArgs()(
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
        input.toString(),
        "--exclude",
        fixSeparatorsInPathPattern("target/nested"),
      ).map { _ =>
        val obtained = dir2string(input)
        assertNoDiff(obtained, expected)
      }
    }

    test(s"scalafmt doesnotexist.scala throws error: $label") {
      def check(filename: String): Unit = {
        def runCli = Cli.mainWithOptions(
          baseCliOptions,
          fixSeparatorsInPathPattern(s"$filename.scala"),
          "--config-str",
          s"""{version="$version",style=IntelliJ}""",
        )
        intercept[Exception](runCli)
      }
      check("notfound")
      check("target/notfound")
    }

    test(s"scalafmt (no matching files) throws error: $label") {
      val scalafmtConfig: Path = PlatformFileOps.mkdtemp(label)
        .resolve(".scalafmt.conf")
      val config: String =
        s"""|
            |version="$version"
            |               """.stripMargin
      PlatformFileOps.writeFile(scalafmtConfig, config)
      val options = baseCliOptions.copy(config = Some(scalafmtConfig))
      intercept[NoMatchingFiles.type](Cli.run(options))
    }

    test(s"scalafmt (no matching files) is okay with --mode diff and --stdin: $label") {
      val stdin = getConfig(
        "--stdin",
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
      ).copy(common = CommonOptions(in = new ByteArrayInputStream("".getBytes)))
      for {
        _ <- runArgs()(
          "--mode",
          "diff",
          "--config-str",
          s"""{version="$version",style=IntelliJ}""",
        )
        _ <- run(stdin)
      } yield {}
    }

    test(s"scalafmt (no arg) read config from git repo: $label") {
      val input = string2dir {
        s"""|/foo.scala
            |object    FormatMe {
            |  val x = 1
            |}
            |/target/foo.scala
            |object A   { }
            |
            |/.scalafmt.conf
            |version = "$version"
            |maxColumn = 2
            |project.excludeFilters = [target]
            |""".stripMargin
      }

      val expected =
        s"""|/.scalafmt.conf
            |version = "$version"
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
      noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
    }

    test(s"scalafmt (no arg, no config): $label") {
      noArgTest(
        string2dir(
          """|/foo.scala
             |object    FormatMe
             |/foo.sc
             |object    FormatMe
             |""".stripMargin,
        ),
        """|/foo.sc
           |object FormatMe
           |
           |/foo.scala
           |object FormatMe
           |""".stripMargin,
        Seq(Array("--config-str", s"""{version="$version"}""")),
      )
    }

    test(s"config is read even from nested dir: $label") {
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
            |version="$version"
            |maxColumn = 2
            |""".stripMargin,
      )
      val workingDir = input / "nested"
      val options: CliOptions = {
        val mock = getMockOptions(input, workingDir)
        mock.copy(common = mock.common.copy(cwd = Some(workingDir)))
      }
      runArgs(options)("foo.scala").map { _ =>
        val obtained = (workingDir / "foo.scala").readFile
        assertNoDiff(obtained, expected)
      }
    }

    test(s"if project.includeFilters isn't modified (and files aren't passed manually), it should ONLY accept scala and sbt files: $label") {
      val root = string2dir {
        s"""|
            |/scalafmt.conf
            |style = default
            |version="$version"
            |/scalafile.scala
            |$unformatted
            |/scalatex.scalatex
            |$unformatted
            |/sbt.sbt
            |$sbtOriginal
            |/sbt.sbtfile
            |$sbtOriginal""".stripMargin
      }

      val config = root / "scalafmt.conf"
      runWith(root, s"--config $config").map { _ =>
        assertNoDiff(dir2string(root / "scalatex.scalatex"), unformatted)
        assertNoDiff(dir2string(root / "sbt.sbtfile"), sbtOriginal)

        assertNoDiff(dir2string(root / "scalafile.scala"), formatted)
        val sbtFormatted =
          """|lazy val x = project
             |lazy val y = project
             |""".stripMargin
        assertNoDiff(dir2string(root / "sbt.sbt"), sbtFormatted)
      }
    }

    test(s"includeFilters are ignored for full paths but NOT test for passed directories: $label") {
      val root = string2dir(
        s"""|
            |/inner1/file1.scala
            |$unformatted
            |/inner2/file2.scalahala
            |$unformatted
            |/inner3/file1.scala
            |$unformatted
            |/inner3/file2.scalahala
            |$unformatted""".stripMargin,
      )
      val inner1 = root / "inner1"
      val inner2 = root / "inner2"
      val inner3 = root / "inner3"
      val full1 = inner3 / "file1.scala"
      val full2 = inner3 / "file2.scalahala"

      val opts = Seq(s"""--config-str {version="$version"}""") ++
        Seq(inner1, inner2, full1, full2)
      runWith(root, opts.mkString(" ")).map { _ =>
        assertNoDiff(dir2string(inner1 / "file1.scala"), formatted)
        assertNoDiff(dir2string(inner2 / "file2.scalahala"), unformatted)
        assertNoDiff(dir2string(full1), formatted)
        assertNoDiff(dir2string(full2), formatted)
      }
    }

    test(s"includeFilters are respected for full paths but NOT test for passed directories: $label") {
      val root = string2dir(
        s"""|
            |/inner1/file1.scala
            |$unformatted
            |/inner2/file2.scalahala
            |$unformatted
            |/inner3/file1.scala
            |$unformatted
            |/inner3/file2.scalahala
            |$unformatted""".stripMargin,
      )
      val inner1 = root / "inner1"
      val inner2 = root / "inner2"
      val inner3 = root / "inner3"
      val full1 = inner3 / "file1.scala"
      val full2 = inner3 / "file2.scalahala"

      val opts = Seq(
        "--respect-project-filters",
        s"""--config-str {version="$version"}""",
      ) ++ Seq(inner1, inner2, full1, full2)
      runWith(root, opts.mkString(" ")).map { _ =>
        assertNoDiff(dir2string(inner1 / "file1.scala"), formatted)
        assertNoDiff(dir2string(inner2 / "file2.scalahala"), unformatted)
        assertNoDiff(dir2string(full1), formatted)
        assertNoDiff(dir2string(full2), unformatted)
      }
    }

    test(s"--config accepts absolute paths: $label") {
      val root = string2dir(
        s"""|/scalafmt.conf
            |version = "$version"
            |style = intellij
            |/foo.scala
            |object    A
            |      """.stripMargin,
      )
      val config = (root / "scalafmt.conf").toString()
      val toFormat = root / "foo.scala"
      Cli.mainWithOptions(baseCliOptions, "--config", config, toFormat.toString())
        .map { _ =>
          val obtained = toFormat.readFile
          assertNoDiff(obtained, "object A\n")
        }
    }

    // These are tests for deprecated flags
    test(s"scalafmt -i -f file1,file2,file3 should still work: $label") {
      val tmpdir = PlatformFileOps.mkdtemp("clitest")
      val file1 = tmpdir.resolve("prefix1.scala")
      val file2 = tmpdir.resolve("prefix2.scala")
      val file3 = tmpdir.resolve("prefix3.scala")
      PlatformFileOps.writeFile(file1, unformatted)
      PlatformFileOps.writeFile(file2, unformatted)
      PlatformFileOps.writeFile(file3, unformatted)
      def fileStr(fs: Path*) = fs.map(_.toFile.getPath).mkString(",")
      runArgs()(
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
        "-i",
        "-f",
        fileStr(file1, file2, file3),
      ).map { _ =>
        val obtained = PlatformFileOps.readFile(file1)
        val obtained2 = PlatformFileOps.readFile(file2)
        val obtained3 = PlatformFileOps.readFile(file3)
        assertNoDiff(obtained, formatted)
        assertNoDiff(obtained2, formatted)
        assertNoDiff(obtained3, formatted)
      }
    }

    test(s"parse error is formatted nicely: $label") {
      val input =
        """|/foo.scala
           |object    A { foo( }
           |""".stripMargin
      noArgTest(
        string2dir(input),
        input,
        Seq(Array("--config-str", s"""{version="$version",style=IntelliJ}""")),
        ExitCode.ParseError,
        assertOut = out =>
          assertContains(
            out,
            s"""foo.scala:1: error:$dialectError illegal start of simple expression
               |object    A { foo( }
               |                   ^""".stripMargin,
          ),
        Some(ExitCode.Ok),
      )
    }

    test(s"command line argument error: $label")(
      Cli.mainWithOptions(getMockOptions(AbsoluteFile.userDir), "--foobar")
        .map(exit => assertEquals(exit, ExitCode.CommandLineArgumentError, exit)),
    )

    test(s"--test failure prints out unified diff: $label") {
      val fooFile = "foo.scala"
      val input =
        s"""|/.scalafmt.conf
            |onTestFailure = "To fix this ..."
            |version = "$version"
            |
            |/$fooFile
            |object    A { }
            |""".stripMargin
      val dir = string2dir(input)
      val fooPath = dir / fooFile
      noArgTest(
        dir,
        input,
        Seq(Array("--test")),
        ExitCode.TestError,
        assertOut = out =>
          assertContains(
            out,
            s"""|b$fooPath
                |@@ -1,1 +1,1 @@
                |-object    A { }
                |+object A {}
                |error: --test failed
                |To fix this ...""".stripMargin,
          ),
      )
    }

    test(s"--test succeeds even with parse error: $label") {
      val input =
        """|/foo.scala
           |object A {
           |""".stripMargin
      noArgTest(
        string2dir(input),
        input,
        Seq(Array("--test", "--config-str", s"""{version="$version"}""")),
        assertOut = out => {
          assertContains(out, s"foo.scala:2: error:$dialectError")
          assertContains(out, "end of file")
          assertContains(out, "error: ParseError=2")
        },
      )
    }

    test(s"--test fails with parse error if fatalWarnings=true: $label") {
      val input =
        s"""|/.scalafmt.conf
            |runner.fatalWarnings = true
            |version = "$version"
            |/foo.scala
            |object A {
            |""".stripMargin
      noArgTest(
        string2dir(input),
        input,
        Seq(Array("--test")),
        ExitCode.ParseError,
        assertOut = out => {
          assertContains(out, s"foo.scala:2: error:$dialectError")
          assertContains(out, "end of file")
          assertContains(out, "error: ParseError=2")
        },
      )
    }

    test(s"exception is thrown on invalid .scalafmt.conf: $label") {
      val input =
        s"""/.scalafmt.conf
           |version="$version"
           |blah = intellij
           |/foo.scala
           |object A {}
           |      """.stripMargin
      noArgTest(
        string2dir(input),
        input,
        Seq(Array.empty),
        ExitCode.UnexpectedError,
        assertOut = out =>
          assert(
            out.contains("Invalid field: blah") ||
              out.contains("found option 'blah' which wasn't expected"),
            s"assertion failed [$out]",
          ),
      )
    }

    test(s"eof: $label") {
      val in = PlatformFileOps.mkdtemp(label).resolve("Foo.scala")
      PlatformFileOps.writeFile(in, "object A")
      val args = Array("--config-str", s"""{version="$version"}""", in.toString)
      Cli.mainWithOptions(baseCliOptions, args: _*).map { exit =>
        assertEquals(exit, ExitCode.Ok)
        val obtained = PlatformFileOps.readFile(in)
        assertEquals(obtained, "object A\n")
      }
    }

    test(s"--config-str should be used if it is specified: $label") {
      val expected = "This message should be shown"
      val unexpected = "This message should not be shown"
      val input =
        s"""|/.scalafmt.conf
            |onTestFailure = "$unexpected"
            |version = "$version"
            |
            |/foo.scala
            |object      A { }
            |""".stripMargin
      noArgTest(
        string2dir(input),
        input,
        Seq(Array(
          "--config-str",
          s"""{version="$version",onTestFailure="$expected"}""",
          "--test",
        )),
        ExitCode.TestError,
        assertOut = out => {
          assertContains(out, expected)
          assertNotContains(out, unexpected)
        },
      )
    }

    test(s"--list enable scalafmt to output a list of unformatted files with ExitCode.TestError: $label") {
      val input =
        s"""|/.scalafmt.conf
            |version = "$version"
            |
            |/bar.scala
            |object    A { }
            |
            |/baz.scala
            |object A {}
            |
            |/dir/foo.scala
            |object   A { }
            |""".stripMargin
      val dir = string2dir(input)
      noArgTest(
        dir,
        input,
        Seq(Array("--list")),
        ExitCode.TestError,
        assertOut = out => {
          assertContains(out, "bar.scala")
          val bslash = PlatformCompat.isNativeOnWindows
          assertContains(out, if (bslash) "dir\\foo.scala" else "dir/foo.scala")
          assertNotContains(out, "baz.scala")
        },
      )
    }
  }
}

class CliTest extends AbstractCliTest with CliTestBehavior {
  if (PlatformCompat.isJVM) testCli("1.6.0-RC4") // test for runDynamic, incompatible with Scala Native
  testCli(stableVersion) // test for runScalafmt

  test(s"path-error") {
    val input =
      s"""|/.scalafmt.conf
          |version = "$stableVersion"
          |project.excludePaths = [
          |    "glob:**/src/main/scala/besom/rpc/**.scala",
          |    "foo.scala"
          |]
          |/foo.scala
          |object    A { foo( }
          |""".stripMargin

    noArgTest(
      string2dir(input),
      input,
      Seq(Array("--test")),
      ExitCode.UnexpectedError,
      assertOut = out =>
        assertContains(
          out,
          s"""Illegal pattern in configuration: foo.scala""".stripMargin,
        ),
      Some(ExitCode.UnexpectedError),
    )

  }

  test(s"regex-error") {
    val input =
      s"""|/.scalafmt.conf
          |version = "$stableVersion"
          |project.excludeFilters = [
          |    ".*foo("
          |]
          |/foo.scala
          |object    A { foo( }
          |""".stripMargin

    noArgTest(
      string2dir(input),
      input,
      Seq(Array("--test")),
      ExitCode.UnexpectedError,
      assertOut = out =>
        assertContains(
          out,
          """|Invalid config: Invalid path patcher regex: /.*foo(/; Unclosed group near index 6
             |.*foo(
             |""".stripMargin,
        ),
      Some(ExitCode.UnexpectedError),
    )

  }

  test("Fail if .scalafmt.conf is missing.") {
    val input =
      s"""|/foo.scala
          |object A {}
          |""".stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(
        Array("--debug"), // debug options is needed to output running scalafmt version
      ),
      ExitCode.UnsupportedVersion,
      assertOut = out => {
        val out1 = out.substring(0, out.length / 2)
        val out2 = out.substring(out1.length)
        assertEquals(out1, out2) // two invocations
        val msg = s"error: missing Scalafmt configuration file."
        assertEquals(out1.substring(0, msg.length), msg)
      },
    )
  }

  test("Fail if `version` setting is missing.") {
    val input =
      s"""|/.scalafmt.conf
          |maxColumn = 10
          |
          |/foo.scala
          |object A {}
          |""".stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(
        Array("--debug"), // debug options is needed to output running scalafmt version
      ),
      ExitCode.UnsupportedVersion,
      assertOut = out => {
        val out1 = out.substring(0, out.length / 2)
        val out2 = out.substring(out1.length)
        assertEquals(out1, out2) // two invocations
        val msg = s"error: missing Scalafmt version."
        assertEquals(out1.substring(0, msg.length), msg)
      },
    )
  }

  test("arguments starting with @ are expanded from a file") {
    val tmpdir = PlatformFileOps.mkdtemp("clitest")
    val argumentsFile = tmpdir.resolve("arguments")
    val configFile = tmpdir.resolve(".scalafmt.conf")
    val arguments = List("--config", configFile.toString, "foobar.scala")
    PlatformFileOps.writeFile(argumentsFile, arguments.mkString("", "\n", "\n"))
    PlatformFileOps
      .writeFile(configFile, List("maxColumn=40").mkString("", "\n", "\n"))
    val obtained = Cli.getConfig(CliTest.defaultOptions, s"@$argumentsFile").get
    val config = obtained.scalafmtConfig.get
    assertEquals(config.maxColumn, 40)
    assertEquals(obtained.customFiles.head.getFileName.toString, "foobar.scala")
  }

  test("can't specify both --config and --config-str") {
    val errStream = new ByteArrayOutputStream()
    val obtained = Console.withErr(errStream)(Cli.getConfig(
      CliTest.defaultOptions,
      "--config",
      "foo",
      "--config-str",
      "bar",
    ))
    assertEquals(
      errStream.toString.trim,
      "Error: may not specify both --config and --config-str",
    )
    assertEquals(None: Option[CliOptions], obtained)
  }

  test(s"scalafmt use includePaths") {
    val input = string2dir {
      s"""|/bar.scala
          |object    FormatMe {
          |  val x = 1
          |}
          |
          |/target/foo.scala
          |object A   { }
          |
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project { includePaths = ["glob:**/bar.scala"] }
          |""".stripMargin
    }

    val expected =
      s"""|/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project { includePaths = ["glob:**/bar.scala"] }
          |
          |/bar.scala
          |object FormatMe {
          |  val x =
          |    1
          |}
          |
          |/target/foo.scala
          |object A   { }
          |""".stripMargin
    noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
  }

  test(s"scalafmt use excludePaths") {
    val input = string2dir {
      s"""|/foo.scala
          |object    FormatMe {
          |  val x = 1
          |}
          |
          |/target/foo.scala
          |object A   { }
          |
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project { excludePaths = ["glob:**target**"] }
          |""".stripMargin
    }

    val expected =
      s"""|/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project { excludePaths = ["glob:**target**"] }
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
    noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
  }

  test(s"scalafmt: includeFilters overrides default includePaths") {
    val input = string2dir {
      s"""|/bar.scala
          |object    FormatMe {
          |  val x = 1
          |}
          |
          |/target/foo.scala
          |object A   { }
          |
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project { includeFilters = ["bar"] }
          |""".stripMargin
    }

    val expected =
      s"""|/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project { includeFilters = ["bar"] }
          |
          |/bar.scala
          |object FormatMe {
          |  val x =
          |    1
          |}
          |
          |/target/foo.scala
          |object A   { }
          |""".stripMargin
    noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
  }

  test(s"scalafmt: includeFilters with explicit includePaths") {
    val defaultIncludePathsJson = ProjectFiles.defaultIncludePaths
      .mkString("[\"", "\", \"", "\"]")
    val input = string2dir {
      s"""|/bar.scala
          |object    FormatMe {
          |  val x = 1
          |}
          |
          |/target/foo.scala
          |object A   { }
          |
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project {
          |  includePaths = $defaultIncludePathsJson
          |  includeFilters = ["bar"]
          |}
          |""".stripMargin
    }

    val expected =
      s"""|/.scalafmt.conf
          |version = $stableVersion
          |maxColumn = 2
          |project {
          |  includePaths = $defaultIncludePathsJson
          |  includeFilters = ["bar"]
          |}
          |
          |/bar.scala
          |object FormatMe {
          |  val x =
          |    1
          |}
          |
          |/target/foo.scala
          |object A {}
          |""".stripMargin
    noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
  }

  test(s"handles .md files when present in includePaths") {
    val input = string2dir {
      s"""|/foobar.md
          |# Hello
          |Example usage 1 with long   spaced line
          |```scala mdoc
          |val  x   =   42
          |```
          |Example usage 2
          |```java
          |val  x   =   42
          |```
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn   = 8
          |project.includePaths."+" = ["glob:**.md"]
          |""".stripMargin
    }
    val expected =
      s"""|
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn   = 8
          |project.includePaths."+" = ["glob:**.md"]
          |
          |/foobar.md
          |# Hello
          |Example usage 1 with long   spaced line
          |```scala mdoc
          |val x =
          |  42
          |```
          |Example usage 2
          |```java
          |val  x   =   42
          |```
          |""".stripMargin

    noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
  }

  test(s"ignores .md files if not present in includePaths") {
    val input = string2dir(
      s"""|/foobar.md
          |# Hello
          |Example usage 1
          |```scala mdoc
          |val  x   =   42
          |```
          |/.scalafmt.conf
          |version = $stableVersion
          |""".stripMargin,
    )
    val expected =
      s"""|
          |/.scalafmt.conf
          |version = $stableVersion
          |
          |/foobar.md
          |# Hello
          |Example usage 1
          |```scala mdoc
          |val x   =   42
          |```
          |""".stripMargin

    noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
      .transform {
        case Success(_) =>
          Try(fail("Should have thrown noMatchingFiles because our markdown file was skipped"))
        case Failure(_: org.scalafmt.Error.NoMatchingFiles.type) => Success(())
        case x => x
      }
  }

  test(s"handles .md with normal comment that contains a nested fence") {
    val input = string2dir(
      s"""|/foobar.md
          | Intro
          |```scala mdoc
          |object    A {
          | /*
          |  * ```scala mdoc
          |  *    val example = "text"
          |  * ```
          |  */
          |                       }
          |```
          |""".stripMargin,
    )
    val expected =
      s"""|/foobar.md
          | Intro
          |```scala mdoc
          |object A {
          |  /*
          |   * ```scala mdoc
          |   *    val example = "text"
          |   * ```
          |   */
          |}
          |```
          |""".stripMargin
    runArgs()(
      input.toString(),
      "--config-str",
      s"""{version = "$stableVersion", project.includePaths."+" = ["glob:**.md"]}""",
    ).map { _ =>
      val obtained = dir2string(input)
      assertNoDiff(obtained, expected)
    }
  }

  // This test might need to change based on maintainer feedback/requirements
  test(s"does apply to .md files with indented fenced content ") {
    val input = string2dir(
      s"""|/foobar2.md
          | Intro text:
          |  ```scala mdoc
          |        object    A {      }
          |  ```
          |""".stripMargin,
    )
    val expected =
      s"""|/foobar2.md
          | Intro text:
          |  ```scala mdoc
          |  object A {}
          |  ```
          |""".stripMargin
    runArgs()(
      input.toString(),
      "--config-str",
      s"""{version = "$stableVersion", project.includePaths."+" = ["glob:**.md"]}""",
    ).map { _ =>
      val obtained = dir2string(input)
      assertNoDiff(obtained, expected)
    }
  }

  // This test might need to change based on maintainer feedback/requirements
  test(s"does not format nested fences when not inside a Scala comment") {
    val input = string2dir(
      s"""|/foobar.md
          |```scala mdoc
          |object    A {
          | /*
          |```scala mdoc
          |   val example = "text"
          |```
          |  */
          |  }
          |```
          |""".stripMargin,
    )
    val expected =
      s"""|/foobar.md
          |```scala mdoc
          |object    A {
          | /*
          |```scala mdoc
          |   val example = "text"
          |```
          |  */
          |  }
          |```
          |""".stripMargin
    runArgs(exitCode = ExitCode.ParseError)(
      input.toString(),
      "--config-str",
      s"""{version = "$stableVersion", project.includePaths."+" = ["glob:**.md"]}""",
    ).map { _ =>
      val obtained = dir2string(input)
      assertNoDiff(obtained, expected)
    }
  }

  test(s"handles .md fences with uneven backticks") {
    val input = string2dir {
      s"""|/foobar.md
          |# Hello
          |Example usage 1 with long   spaced line
          |```scala mdoc
          |val  x   =   42
          |`````
          |Example usage 2
          |```java
          |val  x   =   42
          |```
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn   = 8
          |project.includePaths."+" = ["glob:**.md"]
          |""".stripMargin
    }
    val expected =
      s"""|
          |/.scalafmt.conf
          |version = $stableVersion
          |maxColumn   = 8
          |project.includePaths."+" = ["glob:**.md"]
          |
          |/foobar.md
          |# Hello
          |Example usage 1 with long   spaced line
          |```scala mdoc
          |val x =
          |  42
          |`````
          |Example usage 2
          |```java
          |val  x   =   42
          |```
          |""".stripMargin

    noArgTest(input, expected, Seq(Array.empty[String], Array("--mode", "diff")))
  }

  test("no final EOL") {
    val out = new ByteArrayOutputStream()
    val err = new ByteArrayOutputStream()
    val codeNoEol =
      """|object FormatMe {
         |  val x =
         |    1
         |}""".stripMargin
    val options = getConfig("--stdin", "--test")
    val options2 = options.copy(
      configStr = Some(s"{version = $stableVersion}"),
      common = options.common.copy(
        in = new ByteArrayInputStream(codeNoEol.getBytes),
        out = new PrintStream(out),
        err = new PrintStream(err),
      ),
    )
    run(options2, ExitCode.TestError).map { _ =>
      assertEquals(CliTest.stripCR(out.toString), "error: --test failed\n")
      assertEquals(
        CliTest.stripCR(err.toString),
        """|--- astdin.scala
           |+++ bstdin.scala
           |@@ -4,1 +4,2 @@
           | }
           |+
           |""".stripMargin,
      )
    }
  }

}

object CliTest {

  val stripCR: String => String = {
    val eol = System.lineSeparator()
    if (eol == "\n") identity else _.replace(eol, "\n")
  }

  val defaultOptions = CliOptions.default
    .copy(baseConfig = ScalafmtConfig.default)

}
