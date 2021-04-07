package org.scalafmt.util

import java.nio.file.Paths
import java.util.regex.Pattern

import scala.annotation.tailrec

import metaconfig.Configured
import munit.Assertions._
import org.scalafmt.{Debug, Scalafmt}
import org.scalafmt.config.FormatEvent._
import org.scalafmt.config.{
  Config,
  Indents,
  DanglingParentheses,
  ScalafmtConfig,
  ScalafmtRunner
}
import org.scalafmt.tests.BuildInfo
import org.scalafmt.internal.FormatWriter
import scala.collection.mutable
import scala.meta.Tree
import scala.meta.parsers.Parse
import munit.Location

trait HasTests extends FormatAssertions {
  import HasTests._
  import LoggerOps._

  def scalafmtRunner(sr: ScalafmtRunner, dg: Debug): ScalafmtRunner =
    sr.copy(
      debug = true,
      maxStateVisits = 150000,
      eventCallback = {
        case CreateFormatOps(ops) => dg.formatOps = ops
        case explored: Explored if explored.n % 10000 == 0 =>
          logger.elem(explored)
        case Enqueue(split) => dg.enqueued(split)
        case evt: CompleteFormat => dg.completed(evt)
        case _ =>
      }
    )

  lazy val debugResults = mutable.ArrayBuilder.make[Result]
  val testDir = BuildInfo.resourceDirectory

  def tests: Seq[DiffTest]

  def testsToRun: Seq[DiffTest] = {
    val evalTests = tests
    val onlyTests = evalTests.filter(_.only)
    if (onlyTests.nonEmpty) onlyTests
    else tests
  }

  def isOnly(name: String) = isPrefix(name, onlyPrefix)

  def isSkip(name: String) = isPrefix(name, skipPrefix)

  def stripPrefix(name: String) =
    stripPrefixOpt(name, skipPrefix)
      .orElse(stripPrefixOpt(name, onlyPrefix))
      .getOrElse(name)

  def filename2parse(filename: String): Option[Parse[_ <: Tree]] =
    extension(filename) match {
      case "source" | "scala" | "scalafmt" =>
        Some(scala.meta.parsers.Parse.parseSource)
      case "stat" => Some(scala.meta.parsers.Parse.parseStat)
      case "case" => Some(scala.meta.parsers.Parse.parseCase)
      case _ => None
    }

  def extension(filename: String): String = filename.replaceAll(".*\\.", "")

  def parseDiffTests(content: String, filename: String): Seq[DiffTest] = {
    val sep = {
      if (content.contains(System.lineSeparator)) System.lineSeparator
      else "\n"
    }
    val spec = BuildInfo.resourceDirectory.toPath
      .relativize(Paths.get(filename))
      .getName(0)
      .toString

    val split = content.split(s"(?:^|$sep)<<< ")
    if (split.length <= 1) return Seq.empty // RETURNING!!!

    val (head, tail) = (split.head, split.tail)
    val moduleOnly = isOnly(head)
    val moduleSkip = isSkip(head)

    def loadStyle(content: String, base: ScalafmtConfig): ScalafmtConfig =
      Config.fromHoconString(content, None, base) match {
        case Configured.Ok(c) => c
        case Configured.NotOk(c) =>
          throw new IllegalArgumentException(
            s"""Failed to parse filename $filename:
              |$content
              |$c""".stripMargin
          )
      }
    val style: ScalafmtConfig = loadStyle(
      stripPrefixOpt(head, onlyPrefix).getOrElse(head),
      spec2style(spec)
    )

    @tailrec
    def numLines(str: String, cnt: Int = 1, off: Int = 0): Int = {
      val idx = str.indexOf(sep, off)
      if (idx < 0) cnt else numLines(str, cnt + 1, idx + sep.length)
    }
    var linenum = numLines(head, 2)
    val inputOutputRegex = Pattern.compile(
      s"(.+?)$sep(?:(.+)$sep===$sep)?(.+)$sep>>>(?: +(.+?))?$sep(.*)",
      Pattern.DOTALL
    )
    tail.map { t =>
      val matcher = inputOutputRegex.matcher(t)
      if (!matcher.matches())
        throw new IllegalStateException(
          s"invalid test, missing delimiters:\n$t"
        )
      val name = matcher.group(1)
      val extraConfig = Option(matcher.group(2))
      val original = matcher.group(3)
      val altFilename = Option(matcher.group(4))
      val expected = matcher.group(5)
      val testStyle = extraConfig.fold(style)(loadStyle(_, style))
      val actualName = stripPrefix(name)
      val test = DiffTest(
        actualName,
        altFilename.getOrElse(filename),
        new Location(filename, linenum),
        original,
        trimmed(expected),
        moduleSkip || isSkip(name),
        moduleOnly || isOnly(name),
        testStyle
      )
      linenum += numLines(t)
      test
    }
  }

  private def spec2style(spec: String): ScalafmtConfig =
    spec match {
      case "unit" => HasTests.unitTest40
      case "default" | "standard" | "scala" => HasTests.unitTest80
      case "scalajs" => ScalafmtConfig.scalaJs.copy(maxColumn = 79)
      case "scala3" => ScalafmtConfig.scala3
      case _ => ScalafmtConfig.default
    }

  def saveResult(t: DiffTest, obtained: String, debug: Debug): Result = {
    val output = getFormatOutput(t.style, debug)
    val obtainedHtml = Report.mkHtml(output, t.style)
    Result(
      t,
      obtained,
      obtainedHtml,
      output,
      Option(debug.formatTokenExplored).fold(0)(_.max),
      debug.explored,
      debug.elapsedNs
    )
  }

  def ignore(t: DiffTest): Boolean = false

  def defaultRun(t: DiffTest, parse: Parse[_ <: Tree])(implicit
      loc: Location
  ): Unit = {
    val debug = new Debug(false)
    val runner = scalafmtRunner(t.style.runner, debug).copy(parser = parse)
    val result = Scalafmt
      .formatCode(
        t.original,
        t.style.copy(runner = runner),
        filename = t.filename
      )
    val obtained = result.formatted.get
    if (t.style.rewrite.rules.isEmpty) {
      assertFormatPreservesAst(t.original, obtained)(
        parse,
        result.config.runner.dialect
      )
    }
    assertNoDiff(obtained, t.expected)
  }

  def getFormatOutput(
      style: ScalafmtConfig,
      debug: Debug
  ): Array[FormatOutput] = {
    val builder = mutable.ArrayBuilder.make[FormatOutput]
    val writer = new FormatWriter(debug.formatOps)
    writer.getFormatLocations(debug.state).iterate.foreach { entry =>
      val token = entry.curr.formatToken
      builder += FormatOutput(
        token.left.syntax + entry.getWhitespace(0),
        debug.formatTokenExplored(token.meta.idx)
      )
    }
    builder.result()
  }

  private def trimmed(arg: String): String = arg.trim + "\n"
}

object HasTests {

  private val testing = ScalafmtConfig.default.copy(
    maxColumn = 79,
    assumeStandardLibraryStripMargin = false,
    includeCurlyBraceInSelectChains = false,
    danglingParentheses = DanglingParentheses(false, false),
    align = ScalafmtConfig.default.align.copy(
      tokens = Seq.empty,
      openParenCallSite = true,
      openParenDefnSite = true
    ),
    optIn = ScalafmtConfig.default.optIn.copy(
      breakChainOnFirstMethodDot = false
    ),
    // The new aggressive config style breaks ~40 unit tests. The diff output
    // looks nice, but updating the unit tests would take too much time.
    // I can imagine that I will throw away most of the tests and replace them
    // with autogenerated tests from scala-repos.
    runner = ScalafmtConfig.conservativeRunner
  )

  val unitTest80 = testing.copy(
    indent = Indents(callSite = 4, defnSite = 4)
  )

  val unitTest40 = unitTest80.copy(maxColumn = 39)

  private val onlyPrefix = "ONLY"
  private val skipPrefix = "SKIP"

  private def isPrefix(name: String, prefix: String) =
    name.startsWith(prefix) && !name.charAt(prefix.length).isLetterOrDigit

  private def stripPrefixOpt(name: String, prefix: String) =
    if (isPrefix(name, prefix)) Some(name.substring(prefix.length).trim)
    else None

}
