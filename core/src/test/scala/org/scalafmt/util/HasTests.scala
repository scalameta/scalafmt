package org.scalafmt.util

import java.io.File

import org.scalafmt.Debug
import org.scalafmt.Error.UnknownStyle
import org.scalafmt.FormatEvent.CompleteFormat
import org.scalafmt.FormatEvent.Enqueue
import org.scalafmt.FormatEvent.Explored
import org.scalafmt.FormatEvent.VisitToken
import org.scalafmt.ScalafmtRunner
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtConfig
import org.scalafmt.internal.State
import org.scalatest.FunSuiteLike
import scala.collection.mutable
import scala.meta.Tree
import scala.meta.parsers.Parse
import scala.meta.parsers.ParseException

trait HasTests extends FunSuiteLike with FormatAssertions {
  import LoggerOps._
  val scalafmtRunner = ScalafmtRunner.default.copy(
    debug = true,
    maxStateVisits = 100000,
    eventCallback = {
      case VisitToken(tok) => Debug.visit(tok)
      case explored: Explored if explored.n % 10000 == 0 =>
        logger.elem(explored)
      case Enqueue(split) => Debug.enqueued(split)
      case CompleteFormat(explored, state, tokens) =>
        Debug.explored += explored
        Debug.state = state
        Debug.tokens = tokens
      case _ =>
    }
  )
  val testDir = "core/src/test/resources"

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
    val spec = filename.stripPrefix(testDir + File.separator)
    val moduleOnly = isOnly(content)
    val moduleSkip = isSkip(content)

    content.split("\n<<< ").tail.map { t =>
      val before :: expected :: Nil = t.split("\n>>>\n", 2).toList
      val name :: original :: Nil = before.split("\n", 2).toList
      val actualName = stripPrefix(name)
      DiffTest(spec,
               actualName,
               filename,
               original,
               expected,
               moduleSkip || isSkip(name),
               moduleOnly || isOnly(name),
               file2style(filename))
    }
  }

  def file2style(filename: String): ScalafmtConfig =
    filename.split("/").reverse(1) match {
      case "unit" => ScalafmtConfig.unitTest40
      case "default" | "standard" | "scala" => ScalafmtConfig.unitTest80
      case "scalajs" => ScalafmtConfig.scalaJs
      case "stripMargin" => ScalafmtConfig.default
      case style => throw UnknownStyle(style)
    }

  def saveResult(t: DiffTest, obtained: String, onlyOne: Boolean): Result = {
    val visitedStates = Debug.exploredInTest
    val output = getFormatOutput(t.style, onlyOne)
    val obtainedHtml = Report.mkHtml(output, t.style)
    Result(t,
           obtained,
           obtainedHtml,
           output,
           Debug.maxVisitedToken,
           visitedStates,
           Debug.elapsedNs)
  }

  def ignore(t: DiffTest): Boolean = false

  def runTest(run: (DiffTest, Parse[_ <: Tree]) => Unit)(t: DiffTest): Unit = {
    val paddedName = f"${t.fullName}%-70s|"

    if (ignore(t)) {
      // Not even ignore(t), save console space.
    } else if (t.skip) {
      ignore(paddedName) {}
    } else {
      test(paddedName) {
        Debug.newTest()
        filename2parse(t.filename) match {
          case Some(parse) =>
            try {
              run.apply(t, parse)
            } catch {
              case e: ParseException =>
                fail("test does not parse" +
                    parseException2Message(e, t.original))
            }
          case None => fail(s"Found no parse for filename ${t.filename}")
        }
      }
    }
  }

  def runTestsDefault(): Unit = {
    testsToRun.foreach(runTest(defaultRun))
  }

  def defaultRun(t: DiffTest, parse: Parse[_ <: Tree]): Unit = {
    val runner = scalafmtRunner.withParser(parse)
    val obtained = Scalafmt.format(t.original, t.style, runner).get
    assertFormatPreservesAst(t.original, obtained)(parse)
    assertNoDiff(obtained, t.expected)
  }

  def getFormatOutput(
      style: ScalafmtConfig, onlyOne: Boolean): Array[FormatOutput] = {
    val builder = mutable.ArrayBuilder.make[FormatOutput]()
    State.reconstructPath(
        Debug.tokens, Debug.state.splits, style, debug = onlyOne) {
      case (_, token, whitespace) =>
        builder += FormatOutput(
            token.left.code, whitespace, Debug.formatTokenExplored(token))
    }
    builder.result()
  }
}
