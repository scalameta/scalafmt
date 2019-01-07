package tests

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.nio.file.attribute.FileTime
import org.scalactic.source.Position
import org.scalafmt.dynamic.ScalafmtReporterImpl
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtReporter
import org.scalatest.FunSuite
import scala.collection.mutable
import scala.meta.testkit._

class DynamicSuite extends FunSuite with DiffAssertions {
  class Format(name: String) {
    val download = new ByteArrayOutputStream()
    def downloadLogs: String = download.toString()
    val writer = new PrintWriter(download)
    val out = new ByteArrayOutputStream()
    val parsed = mutable.Map.empty[String, Int]
    def parsedCount: Int = parsed.values.sum
    val reporter: ScalafmtReporter =
      new ScalafmtReporterImpl(new PrintStream(out)) {
        override def parsedConfig(
            config: Path,
            scalafmtVersion: String
        ): Unit = {
          val n = parsed.getOrElse(scalafmtVersion, 0)
          parsed(scalafmtVersion) = n + 1
        }
        override def trimStacktrace(e: Throwable): Unit = {
          e.setStackTrace(
            e.getStackTrace.takeWhile(!_.getClassName.contains("DynamicSuite"))
          )
        }
      }
    var dynamic = Scalafmt
      .create(this.getClass.getClassLoader)
      .withReporter(reporter)
      .withDefaultVersion("1.6.0-RC4")
      .withDownloadWriter(writer)
    def ignoreVersion(): Unit = {
      dynamic = dynamic.withRespectVersion(false)
    }
    def ignoreExcludeFilters(): Unit = {
      dynamic = dynamic.withRespectProjectFilters(false)
    }
    val config = Files.createTempFile("scalafmt", ".scalafmt.conf")
    val filename = Paths.get(name + ".scala")
    var timestamps = 100L
    def setConfig(newConfig: String): Unit = {
      Files.write(this.config, newConfig.getBytes(StandardCharsets.UTF_8))
      timestamps += 100000
      Files.setLastModifiedTime(this.config, FileTime.fromMillis(timestamps))
    }
    def setVersion(newVersion: String): Unit = {
      addConfig(s"version=$newVersion")
    }
    def addConfig(newConfig: String): Unit = {
      Files.write(
        this.config,
        ("\n" + newConfig + "\n").getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.APPEND
      )
    }
    def relevant: String = {
      out.toString.replaceAllLiterally(config.toString, "path/.scalafmt.conf")
    }
    def errors: String = {
      out.toString.lines.filter(_.startsWith("error")).mkString("\n")
    }
    def assertNotIgnored(filename: String)(implicit pos: Position): Unit = {
      assertFormat(
        "object A  {  }",
        "object A {}\n",
        Paths.get(filename)
      )
    }
    def assertIgnored(filename: String): Unit = {
      out.reset()
      val file = Paths.get(filename)
      val original = "object A  { }"
      val obtained = dynamic.format(config, file, original)
      assert(out.toString().contains(s"file excluded: $filename"))
      assertNoDiffOrPrintExpected(obtained, original)
    }
    def assertFormat()(implicit pos: Position): Unit = {
      assertFormat("object A  {  }", "object A {}\n")
    }
    def assertFormat(
        original: String,
        expected: String,
        file: Path = filename
    )(implicit pos: Position): Unit = {
      out.reset()
      val obtained = dynamic.format(config, file, original)
      if (errors.nonEmpty) {
        assertNoDiffOrPrintExpected(out.toString(), "", "Reporter had errors")
      }
      assertNoDiffOrPrintExpected(obtained, expected)
    }
    def assertError(expected: String)(implicit pos: Position): Unit = {
      assertError("object A  {  }", expected)
    }
    def assertError(code: String, expected: String)(
        implicit pos: Position
    ): Unit = {
      out.reset()
      val obtained = dynamic.format(config, filename, code)
      assertNoDiffOrPrintExpected(relevant, expected)
      assertNoDiffOrPrintExpected(obtained, obtained, "Formatter did not error")
    }
  }

  def check(name: String)(fn: Format => Unit): Unit = {
    test(name) {
      val format = new Format(name)
      try fn(format)
      finally format.dynamic.clear()
    }
  }

  def checkVersion(version: String): Unit = {
    check(s"v$version") { f =>
      f.setConfig(
        s"""|version=$version
            |""".stripMargin
      )
      f.assertFormat("object A  {  }", "object A {}\n")
    }
  }

  checkVersion("1.6.0-RC4")
  checkVersion("1.0.0")

  check("filename-ok") { f =>
    f.setConfig("version=1.6.0-RC4")
    f.assertError(
      "object A {",
      """|error: filename-ok.scala:1: error: } expected but end of file found
         |object A {
         |          ^
         |""".stripMargin
    )
  }

  check("filename-missing") { f =>
    f.setConfig("version=1.0.0")
    f.assertError(
      "object A {",
      """|error: filename-missing.scala: <input>:1: error: } expected but end of file found
         |object A {
         |          ^
         |""".stripMargin
    )
  }

  check("missing-version") { f =>
    f.assertError(
      "object A  { }",
      """|
         |error: path/.scalafmt.conf: missing setting 'version'. To fix this problem, add the following line to .scalafmt.conf: 'version=1.6.0-RC4'.
         |""".stripMargin
    )
  }

  check("ignore-version") { f =>
    f.ignoreVersion()
    f.assertFormat(
      "object A  { }",
      "object A {}\n"
    )
  }

  check("excluded-file") { f =>
    f.setConfig(
      """
        |project.includeFilters = [
        |  ".*Spec\\.scala$"
        |]
        |project.excludeFilters = [
        |  "UserSpec\\.scala$"
        |]
        |""".stripMargin
    )
    def check(): Unit = {
      f.assertNotIgnored("path/FooSpec.scala")
      f.assertIgnored("path/App.scala")
      f.assertIgnored("path/UserSpec.scala")
    }
    f.setVersion("1.6.0-RC4")
    check()
    f.setVersion("1.0.0")
    check()
    f.ignoreExcludeFilters()
  }

  check("config-error") { f =>
    f.setConfig(
      """max=70
        |version=1.6.0-RC4
        |""".stripMargin
    )
    f.assertError(
      """|error: path/.scalafmt.conf: Invalid field: max. Expected one of version, maxColumn, docstrings, optIn, binPack, continuationIndent, align, spaces, literals, lineEndings, rewriteTokens, rewrite, indentOperator, newlines, runner, indentYieldKeyword, importSelectors, unindentTopLevelOperators, includeCurlyBraceInSelectChains, assumeStandardLibraryStripMargin, danglingParentheses, poorMansTrailingCommasInConfigStyle, trailingCommas, verticalMultilineAtDefinitionSite, verticalMultilineAtDefinitionSiteArityThreshold, verticalMultiline, onTestFailure, encoding, project
         |""".stripMargin
    )
  }

  check("config-cache") { f =>
    f.setConfig("version=1.6.0-RC4")
    f.assertFormat()
    f.assertFormat()
    assert(f.parsedCount == 1, f.parsed)
    f.setConfig(
      """version=1.6.0-RC4
        |maxColumn = 40
        |""".stripMargin
    )
    f.assertFormat()
    assert(f.parsedCount == 2, f.parsed)
    f.assertFormat()
    assert(f.parsedCount == 2, f.parsed)
    f.setConfig(
      """version=1.0.0
        |maxColumn = 40
        |""".stripMargin
    )
    f.assertFormat()
    assert(f.parsed == Map("1.0.0" -> 1, "1.6.0-RC4" -> 3))
  }

  check("wrong-version") { f =>
    f.setVersion("1.0")
    f.assertError(
      "error: failed to resolve Scalafmt version '1.0'"
    )
    assert(f.downloadLogs.nonEmpty)
  }

  check("sbt") { f =>
    def check(isLegacy: Boolean): Unit = {
      List("build.sbt", "build.sc").foreach { filename =>
        f.assertFormat(
          "lazy   val   x =  project",
          "lazy val x = project\n",
          Paths.get(filename)
        )
        val input = if (isLegacy) ": <input>" else ""
        f.assertError(
          "lazy   val   x =  project",
          s"""|error: sbt.scala$input:1: error: classes cannot be lazy
              |lazy   val   x =  project
              |^
              |""".stripMargin
        )
      }
    }
    f.setConfig(
      """|project.includeFilters = [ ".*" ]
         |""".stripMargin
    )
    f.setVersion("1.6.0-RC4")
    check(isLegacy = false)
    f.setVersion("1.2.0")
    check(isLegacy = true)
  }

}
