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
import org.scalafmt.dynamic.ConsoleScalafmtReporter
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtReporter
import org.scalatest.FunSuite
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.{meta => m}
import scala.meta.testkit._
import PositionSyntax._
import org.scalafmt.interfaces.PositionException

class DynamicSuite extends FunSuite with DiffAssertions {
  class Format(name: String) {
    val download = new ByteArrayOutputStream()
    def downloadLogs: String = download.toString()
    val out = new ByteArrayOutputStream()
    val parsed = mutable.Map.empty[String, Int]
    def parsedCount: Int = parsed.values.sum
    val missingVersions = ListBuffer.empty[String]
    val reporter: ScalafmtReporter =
      new ConsoleScalafmtReporter(new PrintStream(out)) {
        override def downloadWriter(): PrintWriter = new PrintWriter(download)

        override def error(file: Path, e: Throwable): Unit = e match {
          case p: PositionException =>
            val input = m.Input.VirtualFile(file.toString, p.code)
            val pos =
              m.Position.Range(
                input,
                p.startLine,
                p.startCharacter,
                p.endLine,
                p.endCharacter
              )
            val formattedMessage = pos.formatMessage("error", p.shortMessage)
            out.write(formattedMessage.getBytes(StandardCharsets.UTF_8))
          case _ =>
            super.error(file, e)
        }
        override def missingVersion(
            config: Path,
            defaultVersion: String
        ): Unit = {
          missingVersions += defaultVersion
        }
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
      .withDefaultVersion(latest)
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
    def assertMissingVersion()(implicit pos: Position): Unit = {
      out.reset()
      missingVersions.clear()
      val original = "object  A"
      val obtained = dynamic.format(config, filename, original)
      assert(out.toString().isEmpty)
      assert(missingVersions.nonEmpty)
      assertNoDiffOrPrintExpected(obtained, original, "Formatter did not error")
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

  def latest = "2.0.0-RC1"

  def checkVersion(version: String): Unit = {
    check(s"v$version") { f =>
      f.setConfig(
        s"""|version=$version
            |""".stripMargin
      )
      f.assertFormat("object A  {  }", "object A {}\n")
    }
  }

  checkVersion(latest)
  checkVersion("1.0.0")

  check("parse-error") { f =>
    def check(): Unit = {
      f.assertError(
        "object object A",
        """|parse-error.scala:1:8: error: identifier expected but object found
           |object object A
           |       ^^^^^^
           |""".stripMargin
      )
    }
    f.setVersion(latest)
    check()
    f.setVersion("1.0.0")
    check()
  }

  check("missing-version") { f =>
    f.assertMissingVersion()
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
    f.setVersion(latest)
    check()
    f.setVersion("1.0.0")
    check()
    f.ignoreExcludeFilters()
  }

  check("config-error") { f =>
    f.setConfig(
      s"""max=70
         |version=$latest
         |""".stripMargin
    )
    f.assertError(
      """|error: path/.scalafmt.conf: Invalid field: max. Expected one of version, maxColumn, docstrings, optIn, binPack, continuationIndent, align, spaces, literals, lineEndings, rewriteTokens, rewrite, indentOperator, newlines, runner, indentYieldKeyword, importSelectors, unindentTopLevelOperators, includeCurlyBraceInSelectChains, includeNoParensInSelectChains, assumeStandardLibraryStripMargin, danglingParentheses, poorMansTrailingCommasInConfigStyle, trailingCommas, verticalMultilineAtDefinitionSite, verticalMultilineAtDefinitionSiteArityThreshold, verticalMultiline, onTestFailure, encoding, project
         |""".stripMargin
    )
  }

  check("config-cache") { f =>
    f.setVersion(latest)
    f.assertFormat()
    f.assertFormat()
    assert(f.parsedCount == 1, f.parsed)
    f.setConfig(
      s"""version=$latest
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
    assert(f.parsed == Map("1.0.0" -> 1, latest -> 3))
  }

  check("wrong-version") { f =>
    f.setVersion("1.0")
    f.assertError(
      "error: path/.scalafmt.conf: failed to resolve Scalafmt version '1.0'"
    )
    assert(f.downloadLogs.nonEmpty)
  }

  check("sbt") { f =>
    def check(): Unit = {
      List("build.sbt", "build.sc").foreach { filename =>
        f.assertFormat(
          "lazy   val   x =  project",
          "lazy val x = project\n",
          Paths.get(filename)
        )
        f.assertError(
          "lazy   val   x =  project",
          """|sbt.scala:1:1: error: classes cannot be lazy
             |lazy   val   x =  project
             |^^^^
             |""".stripMargin
        )
      }
    }
    f.setConfig(
      """|project.includeFilters = [ ".*" ]
         |""".stripMargin
    )
    f.setVersion(latest)
    check()
    f.setVersion("1.2.0")
    check()
  }

  check("no-config") { f =>
    Files.delete(f.config)
    f.assertError("error: path/.scalafmt.conf: file does not exist")
  }
}
