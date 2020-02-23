package org.scalafmt.util

import java.io.File

import scala.annotation.tailrec

import metaconfig.Configured
import org.scalafmt.Error.UnknownStyle
import org.scalafmt.{Debug, Scalafmt}
import org.scalafmt.config.FormatEvent._
import org.scalafmt.config.{
  Config,
  ContinuationIndent,
  DanglingParentheses,
  ScalafmtConfig,
  ScalafmtRunner
}
import org.scalafmt.internal.FormatWriter

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.parsers.Parse
import scala.util.Try

import org.scalactic.source.Position

trait HasTests extends FormatAssertions {
  import LoggerOps._

  def scalafmtRunner(base: ScalafmtRunner): ScalafmtRunner = base.copy(
    debug = true,
    maxStateVisits = 150000,
    eventCallback = {
      case CreateFormatOps(ops) => Debug.formatOps = ops
      case explored: Explored if explored.n % 10000 == 0 =>
        logger.elem(explored)
      case Enqueue(split) => Debug.enqueued(split)
      case evt: CompleteFormat => Debug.completed(evt)
      case _ =>
    }
  )
  lazy val debugResults = mutable.ArrayBuilder.make[Result]
  val testDir = new File(getClass.getClassLoader.getResource("").toURI).getAbsolutePath

  def tests: Seq[DiffTest]

  def testsToRun: Seq[DiffTest] = {
    val evalTests = tests
    val onlyTests = evalTests.filter(_.only)
    if (onlyTests.nonEmpty) onlyTests
    else tests
  }

  def isOnly(name: String) = name.startsWith("ONLY ")

  def isSkip(name: String) = name.startsWith("SKIP ")

  def stripPrefix(name: String) =
    name.stripPrefix("SKIP ").stripPrefix("ONLY ").trim

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
    val sep =
      if (content.contains(System.lineSeparator)) System.lineSeparator
      else "\n"
    val spec = filename.stripPrefix(testDir + File.separator)
    val moduleOnly = isOnly(content)
    val moduleSkip = isSkip(content)
    val split = content.split(s"$sep<<< ")

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
    val style: ScalafmtConfig = {
      val pathPatternToReplace =
        if (OsSpecific.isWindows) s"""\\\\.*""" else "/.*"
      Try(
        spec2style(spec.replaceFirst(pathPatternToReplace, ""))
      ).getOrElse(
        loadStyle(split.head.stripPrefix("ONLY "), ScalafmtConfig.default)
      )
    }

    @tailrec
    def numLines(str: String, cnt: Int = 1, off: Int = 0): Int = {
      val idx = str.indexOf(sep, off)
      if (idx < 0) cnt else numLines(str, cnt + 1, idx + sep.length)
    }
    var linenum = numLines(split.head, 2)
    split.tail.map { t =>
      val before :: expected :: Nil = t.split(s"$sep>>>$sep", 2).toList
      val extraConfig = before.split(s"$sep===$sep", 2).toList
      val (testStyle, name :: original :: Nil) = extraConfig match {
        case nameAndOriginal :: Nil =>
          (style, nameAndOriginal.split(sep, 2).toList)
        case nameAndConfig :: original :: Nil =>
          val name :: config :: Nil = nameAndConfig.split(sep, 2).toList
          (loadStyle(config, style), name :: original :: Nil)
      }
      val actualName = stripPrefix(name)
      val test = DiffTest(
        actualName,
        new Position(spec, filename, linenum),
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

  def spec2style(spec: String): ScalafmtConfig =
    spec match {
      case "unit" => HasTests.unitTest40
      case "default" | "standard" | "scala" => HasTests.unitTest80
      case "scalajs" => ScalafmtConfig.scalaJs.copy(maxColumn = 79)
      case style => throw UnknownStyle(style)
    }

  def saveResult(t: DiffTest, obtained: String, onlyOne: Boolean): Result = {
    val visitedStates = Debug.exploredInTest
    val output = getFormatOutput(t.style, onlyOne)
    val obtainedHtml = Report.mkHtml(output, t.style)
    Result(
      t,
      obtained,
      obtainedHtml,
      output,
      Debug.formatTokenExplored.max,
      visitedStates,
      Debug.elapsedNs
    )
  }

  def ignore(t: DiffTest): Boolean = false

  def defaultRun(t: DiffTest, parse: Parse[_ <: Tree]): Unit = {
    implicit val loc: Position = t.loc
    val runner = scalafmtRunner(t.style.runner).copy(parser = parse)
    val obtained = Scalafmt
      .formatCode(
        t.original,
        t.style.copy(runner = runner),
        filename = loc.filePathname
      )
      .get
    if (t.style.rewrite.rules.isEmpty) {
      assertFormatPreservesAst(t.original, obtained)(
        parse,
        t.style.runner.dialect
      )
    }
    assertNoDiff(obtained, t.expected)
  }

  def getFormatOutput(
      style: ScalafmtConfig,
      onlyOne: Boolean
  ): Array[FormatOutput] = {
    val builder = mutable.ArrayBuilder.make[FormatOutput]
    val locations = new FormatWriter(Debug.formatOps)
      .getFormatLocations(Debug.state, debug = onlyOne)
    locations.iterate.foreach { entry =>
      val token = entry.curr.formatToken
      builder += FormatOutput(
        token.left.syntax + entry.getWhitespace(0),
        Debug.formatTokenExplored(token.meta.idx)
      )
    }
    if (onlyOne) {
      locations.locations.lastOption.foreach { location =>
        logger.debug(s"Total cost: ${location.state.cost}")
      }
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
    continuationIndent = ContinuationIndent(4, 4)
  )

  val unitTest40 = unitTest80.copy(maxColumn = 39)

}
