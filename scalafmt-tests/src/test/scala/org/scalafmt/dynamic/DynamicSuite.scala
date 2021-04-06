package org.scalafmt.dynamic

import java.io.{ByteArrayOutputStream, PrintStream, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.nio.file.attribute.FileTime

import org.scalafmt.interfaces.{PositionException, Scalafmt, ScalafmtReporter}
import PositionSyntax._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.{meta => m}
import munit.FunSuite
import munit.Location

class DynamicSuite extends FunSuite {
  class Format(name: String, cfgFunc: ScalafmtDynamic => ScalafmtDynamic) {
    val download = new ByteArrayOutputStream()
    def downloadLogs: String = download.toString()
    val out = new ByteArrayOutputStream()
    val parsed = mutable.Map.empty[String, Int]
    def parsedCount: Int = parsed.values.sum
    val missingVersions = ListBuffer.empty[String]
    val reporter: ScalafmtReporter =
      new ConsoleScalafmtReporter(new PrintStream(out)) {
        override def downloadWriter(): PrintWriter = new PrintWriter(download)

        override def error(file: Path, e: Throwable): Unit =
          e match {
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
    val dynamic: ScalafmtDynamic = cfgFunc(
      Scalafmt
        .create(this.getClass.getClassLoader)
        .withReporter(reporter)
        .withDefaultVersion(latest)
        .asInstanceOf[ScalafmtDynamic]
    )
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
      out.toString.linesIterator
        .filter(_.startsWith("error"))
        .mkString("\n")
    }
    def assertNotIgnored(filename: String)(implicit loc: Location): Unit = {
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
      val outString = out.toString().replaceAll("\\\\", "/")
      assert(outString.contains(s"file excluded: $filename"))
      assertNoDiff(obtained, original)
    }
    def assertFormat()(implicit loc: Location): Unit = {
      assertFormat("object A  {  }", "object A {}\n")
    }
    def assertFormat(
        original: String,
        expected: String,
        file: Path = filename
    )(implicit loc: Location): Unit = {
      out.reset()
      val obtained = dynamic.format(config, file, original)
      if (errors.nonEmpty) {
        assertNoDiff(out.toString(), "", "Reporter had errors")
      }
      assertNoDiff(obtained, expected)
    }
    def assertMissingVersion()(implicit loc: Location): Unit = {
      out.reset()
      missingVersions.clear()
      intercept[ScalafmtDynamicError.ConfigMissingVersion] {
        dynamic.format(config, filename, "object  A")
      }
      assert(out.toString().isEmpty)
      assert(missingVersions.nonEmpty)
    }
    def assertThrows[A <: Throwable: ClassTag](
        code: String = "object A  {  }"
    )(implicit loc: Location): A = {
      out.reset()
      intercept[A] {
        dynamic.format(config, filename, code)
      }
    }
    def assertError(expected: String)(implicit loc: Location): Unit = {
      assertError("object A  {  }", expected)
    }
    def assertError(
        code: String,
        expected: String,
        path: Path = filename
    )(implicit loc: Location): Unit = {
      out.reset()
      val obtained = dynamic.format(config, path, code)
      assertNoDiff(relevant, expected)
      assertNoDiff(obtained, obtained, "Formatter did not error")
    }
  }

  def check(
      name: String,
      cfgFunc: ScalafmtDynamic => ScalafmtDynamic = identity
  )(fn: Format => Unit): Unit = {
    test(name) {
      val format = new Format(name, cfgFunc)
      try fn(format)
      finally format.dynamic.clear()
    }
  }

  private val testedVersions = Seq(
    "2.5.3",
    "2.0.0-RC4",
    "1.6.0-RC4",
    "1.5.1",
    "1.5.0",
    "1.4.0",
    "1.3.0",
    "1.2.0",
    "1.1.0",
    "1.0.0-RC4",
    "1.0.0"
  )

  def checkExhaustive(name: String)(fn: (Format, String) => Unit): Unit = {
    testedVersions.foreach { version =>
      test(s"$name (version: $version)") {
        val format = new Format(name, identity)
        try fn(format, version)
        finally format.dynamic.clear()
      }
    }
  }

  def latest = "2.5.3"

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
  checkVersion("1.5.1")
  checkVersion("1.0.0")
  //checkVersion("0.2.8") // fails for now

  check("parse-error") { f =>
    def check(version: String): Unit = {
      f.setVersion(version)
      f.assertError(
        "object object A",
        """|parse-error.scala:1:8: error: identifier expected but object found
          |object object A
          |       ^^^^^^""".stripMargin
      )
    }
    check(latest)
    check("1.0.0")
  }

  check("missing-version") { f => f.assertMissingVersion() }

  check("ignore-version", _.withRespectVersion(false)) { f =>
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
    def check(version: String): Unit = {
      f.setVersion(version)
      f.assertNotIgnored("path/FooSpec.scala")
      f.assertIgnored("path/App.scala")
      f.assertIgnored("path/UserSpec.scala")
    }
    check(latest)
    check("1.0.0")
  }

  check("ignore-exclude-filters", _.withRespectProjectFilters(false)) { f =>
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
    def check(version: String): Unit = {
      f.setVersion(version)
      f.assertNotIgnored("path/App.pm")
      f.assertNotIgnored("path/App.scala")
      f.assertNotIgnored("path/UserSpec.scala")
    }
    check(latest)
  }

  check("config-error") { f =>
    f.setConfig(
      s"""max=70
        |version=$latest
        |""".stripMargin
    )
    val thrown = f.assertThrows[ScalafmtDynamicError.ConfigParseError]()
    assert(thrown.getMessage.contains("Invalid config: Invalid field: max."))
  }

  check("config-cache") { f =>
    f.setVersion(latest)
    f.assertFormat()
    f.assertFormat()
    assertEquals(f.parsedCount, 1, f.parsed)

    f.setConfig("invalid")
    val parseError = f.assertThrows[ScalafmtDynamicError.ConfigParseError]()
    assert(
      parseError.getMessage
        .contains("Key 'invalid' may not be followed by token: end of file")
    )

    f.setConfig("maxColumn = 40")
    f.assertMissingVersion()

    f.setConfig(
      s"""version=$latest
        |maxColumn = 40
        |""".stripMargin
    )
    f.assertFormat()
    assertEquals(f.parsedCount, 2, f.parsed)
    f.assertFormat()
    assertEquals(f.parsedCount, 2, f.parsed)

    f.setConfig(
      """version=1.0.0
        |maxColumn = 40
        |""".stripMargin
    )
    f.assertFormat()
    assertEquals(f.parsedCount, 3, f.parsed)
    f.assertFormat()
    assertEquals(f.parsedCount, 3, f.parsed)

    assertEquals(f.parsed.toMap, Map("1.0.0" -> 1, latest -> 2))
  }

  check("wrong-version") { f =>
    f.setVersion("1.0")
    val error = f.assertThrows[ScalafmtDynamicError.ConfigInvalidVersion]()
    assert(error.getMessage == "Invalid version: 1.0")
    assert(f.downloadLogs.isEmpty)
  }

  check("sbt") { f =>
    def check(version: String): Unit = {
      f.setVersion(version)
      List("build.sbt", "build.sc").foreach { filename =>
        val path = Paths.get(filename)
        // test sbt allows top-level terms
        f.assertFormat(
          "lazy   val   x =  project",
          "lazy val x = project\n",
          path
        )
        // test scala doesn't allow top-level terms (not passing path here)
        f.assertError(
          "lazy   val   x =  project",
          """|sbt.scala:1:1: error: classes cannot be lazy
            |lazy   val   x =  project
            |^^^^""".stripMargin
        )
        // check wrapped literals, supported in sbt using scala 2.13+
        val wrappedLiteral = "object a { val  x:  Option[0]  =  Some(0) }"
        def isWrappedLiteralFailure: Unit =
          f.assertError(
            wrappedLiteral,
            s"""$filename:1:28: error: identifier expected but integer constant found
              |$wrappedLiteral
              |                           ^""".stripMargin,
            path
          )
        def isWrappedLiteralSuccess: Unit =
          f.assertFormat(
            wrappedLiteral,
            wrappedLiteral.replaceAll("  +", " ").trim + "\n",
            path
          )
        if (version > "2.0")
          isWrappedLiteralSuccess
        else
          isWrappedLiteralFailure
      }
    }
    f.setConfig(
      """|project.includeFilters = [ ".*" ]
        |""".stripMargin
    )
    check(latest)
    check("1.2.0")
  }

  check("no-config") { f =>
    Files.delete(f.config)
    val thrown = f.assertThrows[ScalafmtDynamicError.ConfigDoesNotExist]()
    assert(thrown.getMessage.contains("Missing config"))
  }

  check("intellij-default-config") { f: Format =>
    val version = ScalafmtVersion(1, 5, 1)
    f.setVersion(version.toString)
    f.assertFormat()

    val reflect = f.dynamic.formatCache.getFromCache(version)
    assert(reflect.nonEmpty)
    assert(reflect.get.right.get.intellijScalaFmtConfig.nonEmpty)
  }

  checkExhaustive("continuation-indent-callSite-and-defnSite") { (f, version) =>
    f.setConfig(
      s"""version=$version
        |continuationIndent.callSite = 5
        |continuationIndent.defnSite = 3
      """.stripMargin
    )
    val original =
      """class A {
        |  function1(
        |  argument1,
        |  ""
        |  )
        |
        |  def function2(
        |  argument1: Type1
        |  ): ReturnType
        |}
      """.stripMargin
    val expected =
      """class A {
        |  function1(
        |       argument1,
        |       ""
        |  )
        |
        |  def function2(
        |     argument1: Type1
        |  ): ReturnType
        |}
        |""".stripMargin
    f.assertFormat(original, expected)
  }

  checkExhaustive("hasRewriteRules-and-withoutRewriteRules") { (f, version) =>
    f.setConfig(
      s"""version=$version
        |rewrite.rules = [RedundantBraces]
        """.stripMargin
    )
    f.assertFormat()
    val configOpt = f.dynamic.configsCache
      .getFromCache(f.config)
      .collect { case Right((cfg, _)) => cfg }
    assert(configOpt.nonEmpty)
    val config = configOpt.get
    assert(config.hasRewriteRules)
    val configWithoutRewrites = config.withoutRewriteRules
    assertNotEquals(config, configWithoutRewrites)
    assert(!configWithoutRewrites.hasRewriteRules)
  }
}
