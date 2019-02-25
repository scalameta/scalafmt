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

import org.scalafmt.Versions

abstract class AbstractCliTest extends FunSuite with DiffAssertions {
  def mkArgs(str: String): Array[String] =
    str.split(' ')

  def runWith(root: AbsoluteFile, argStr: String): Unit = {
    val args = mkArgs(argStr)
    val opts = getMockOptions(root)

    val conf = Cli.getConfig(args, opts)
    Cli.run(conf.get)
  }

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
      val configTest = Cli.getConfig(Array("--test"), config).get
      Cli.run(configTest)
      assertOut(out.toString())
    }

  }

}

trait CliTestBehavior { this: AbstractCliTest =>
  def testCli(version: String) {
    val label = if (version == Versions.version) "core" else "dynamic"
    test(s"scalafmt tmpFile tmpFile2: $label") {
      val originalTmpFile = Files.createTempFile("prefix", ".scala")
      val originalTmpFile2 = Files.createTempFile("prefix2", ".scala")
      val scalafmtConfig = Files.createTempFile("scalafmtConfig", ".scala")
      val config = s"""
                      |version="$version"
                      |maxColumn=7
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

    test(s"scalafmt --stdout tmpFile prints to stdout: $label") {
      val originalTmpFile = Files.createTempFile("prefix", ".scala")
      Files.write(originalTmpFile, unformatted.getBytes)
      val args = Array(
        "--stdout",
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
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

    test(s"scalafmt --stdin --assume-filename: $label") {
      val scalafmtConfig = Files.createTempFile(".scalafmt", ".conf")
      val config = s"""
                      |version="$version"
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
        )
      )
      val obtained = new String(baos.toByteArray, StandardCharsets.UTF_8)
      assertNoDiff(obtained, sbtExpected)
      assert(obtained.size == sbtExpected.size)
    }

    test(s"scalafmt --test tmpFile is left unformmated: $label") {
      val tmpFile = Files.createTempFile("prefix", ".scala")
      Files.write(tmpFile, unformatted.getBytes)
      val args = Array(
        tmpFile.toFile.getPath,
        "--test",
        "--config-str",
        s"""{version="$version",style=IntelliJ}"""
      )
      val formatInPlace = getConfig(args)
      val exit = Cli.run(formatInPlace)
      assert(exit.is(ExitCode.TestError))
      val str = FileOps.readFile(tmpFile.toString)
      assertNoDiff(str, unformatted)
    }

    test(s"scalafmt foo.randomsuffix is formatted: $label") {
      val tmpFile = Files.createTempFile("prefix", "randomsuffix")
      Files.write(tmpFile, unformatted.getBytes)
      val args = Array(
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
        tmpFile.toFile.getAbsolutePath
      )
      Cli.exceptionThrowingMain(args)
      val obtained = FileOps.readFile(tmpFile.toString)
      // TODO: We need to pass customFiles information to ProjectFiles
      assertNoDiff(obtained, formatted)
    }

    test(s"handles .scala, .sbt, and .sc files: $label") {
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
          "--config-str",
          s"""{version="$version",style=IntelliJ}"""
        )
      )
      Cli.run(options)
      val obtained = dir2string(input)
      assertNoDiff(obtained, expected)
    }

    test(s"excludefilters are respected: $label") {
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
          "--config-str",
          s"""{version="$version",style=IntelliJ}""",
          input.path,
          "--exclude",
          "target/nested".asFilename
        )
      )
      Cli.run(options)
      val obtained = dir2string(input)
      assertNoDiff(obtained, expected)
    }

    test(s"scalafmt doesnotexist.scala throws error: $label") {
      def check(filename: String): Unit = {
        val args = Array(
          s"$filename.scala".asFilename,
          "--config-str",
          s"""{version="$version",style=IntelliJ}"""
        )
        intercept[IOException] {
          Cli.exceptionThrowingMain(args)
        }
      }
      check("notfound")
      check("target/notfound")
    }

    test(s"scalafmt (no matching files) throws error: $label") {
      val scalafmtConfig: Path = Files.createTempFile(".scalafmt", ".conf")
      val config: String = s"""
                              |version="$version"
               """.stripMargin
      Files.write(scalafmtConfig, config.getBytes)
      val options = baseCliOptions.copy(config = Some(scalafmtConfig))
      intercept[NoMatchingFiles.type] {
        Cli.run(options)
      }
    }

    test(
      s"scalafmt (no matching files) is okay with --diff and --stdin: $label") {
      val diff = getConfig(
        Array(
          "--diff",
          "--config-str",
          s"""{version="$version",style=IntelliJ}"""
        )
      )
      val stdin = getConfig(
        Array(
          "--stdin",
          "--config-str",
          s"""{version="$version",style=IntelliJ}"""
        )
      ).copy(
        common = CommonOptions(in = new ByteArrayInputStream("".getBytes))
      )
      Cli.run(diff)
      Cli.run(stdin)
    }

    test(s"scalafmt (no arg) read config from git repo: $label") {
      val input = string2dir(
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
      )

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
      noArgTest(
        input,
        expected,
        Seq(Array.empty[String], Array("--diff"))
      )
    }
    test(s"scalafmt (no arg, no config): $label") {
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
          Array("--config-str", s"""{version="$version"}""")
        )
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
      s"if project.includeFilters isn't modified (and files aren't passed manually), it should ONLY accept scala and sbt files: $label"
    ) {
      val root =
        string2dir(
          s"""
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
      s"includeFilters are ignored for full paths but NOT ignore for passed directories: $label"
    ) {
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
        s"""--config-str {version="$version"} $inner1 $inner2 $full""")

      assertNoDiff(inner1 / "file1.scala", formatted)
      assertNoDiff(inner2 / "file2.scalahala", unformatted)
      assertNoDiff(full, formatted)
    }

    test(s"--config accepts absolute paths: $label") {
      val root = string2dir(
        s"""/scalafmt.conf
           |version = "$version"
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
    test(s"scalafmt -i -f file1,file2,file3 should still work: $label") {
      val file1 = Files.createTempFile("prefix", ".scala")
      val file2 = Files.createTempFile("prefix2", ".scala")
      val file3 = Files.createTempFile("prefix3", ".scala")
      Files.write(file1, unformatted.getBytes)
      Files.write(file2, unformatted.getBytes)
      Files.write(file3, unformatted.getBytes)
      def fileStr(fs: Path*) = fs.map(_.toFile.getPath).mkString(",")
      val args = Array(
        "--config-str",
        s"""{version="$version",style=IntelliJ}""",
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

    test(s"parse error is formatted nicely: $label") {
      val input =
        """|/foo.scala
           |object    A { foo( }
           |""".stripMargin
      noArgTest(
        string2dir(input),
        input,
        Seq(
          Array(
            "--config-str",
            s"""{version="$version",style=IntelliJ}"""
          )
        ),
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

    test(s"command line argument error: $label") {
      val exit = Cli.mainWithOptions(
        Array("--foobar"),
        getMockOptions(AbsoluteFile.userDir)
      )
      assert(exit.is(ExitCode.CommandLineArgumentError), exit)
    }

    test(s"--test failure prints out unified diff: $label") {
      val input =
        s"""|/.scalafmt.conf
            |onTestFailure = "To fix this ..."
            |version = "$version"
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

    test(s"--test succeeds even with parse error: $label") {
      val input =
        """|/foo.scala
           |object A {
           |""".stripMargin
      noArgTest(
        string2dir(input),
        input,
        Seq(Array("--test", "--config-str", s"""{version="$version"}""")),
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

    test(s"exception is thrown on invalid .scalafmt.conf: $label") {
      val input =
        s"""/.scalafmt.conf
           |version="$version"
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

    test(s"eof: $label") {
      val in = Files.createTempFile("scalafmt", "Foo.scala")
      Files.write(in, "object A".getBytes(StandardCharsets.UTF_8))
      val exit = Cli.mainWithOptions(Array(in.toString), CliOptions.default)
      assert(exit.isOk)
      val obtained = new String(Files.readAllBytes(in), StandardCharsets.UTF_8)
      assert(obtained == "object A\n")
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
        Seq(
          Array(
            "--config-str",
            s"""{version="$version",onTestFailure="$expected"}""",
            "--test")),
        assertExit = { exit =>
          assert(exit.is(ExitCode.TestError))
        },
        assertOut = out => {
          assert(
            out.contains(expected) &&
              !out.contains(unexpected)
          )
        }
      )
    }
  }
}

class CliTest extends AbstractCliTest with CliTestBehavior {
  testsFor(testCli("1.6.0-RC4")) // test for runDynamic
  testsFor(testCli(Versions.version)) // test for runScalafmt

  test("Running pre-resolved version of scalafmt if .scalafmt.conf is missing.") {
    val input =
      s"""|/foo.scala
          |object A {}
          |""".stripMargin
    noArgTest(
      string2dir(input),
      input,
      Seq(
        Array("--debug") // debug options is needed to output running scalafmt version
      ),
      assertExit = { exit =>
        assert(exit.isOk, exit)
      },
      assertOut = out => {
        assert(out.contains(Versions.version))
      }
    )
  }

  test(
    "Running pre-resolved version of scalafmt if `version` setting is missing.") {
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
        Array("--debug") // debug options is needed to output running scalafmt version
      ),
      assertExit = { exit =>
        assert(exit.isOk, exit)
      },
      assertOut = out => {
        assert(out.contains(Versions.version))
      }
    )
  }
}
