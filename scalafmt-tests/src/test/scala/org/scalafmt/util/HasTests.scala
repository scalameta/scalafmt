package org.scalafmt.util

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.parsers.Parse
import scala.meta.parsers.ParseException
import scala.util.Try

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import metaconfig.Configured
import org.scalafmt.Debug
import org.scalafmt.Error.UnableToFindStyle
import org.scalafmt.Error.UnknownStyle
import org.scalafmt.Scalafmt
import org.scalafmt.config.AlignToken
import org.scalafmt.config.BinPack
import org.scalafmt.config.Config
import org.scalafmt.config.FormatEvent.CompleteFormat
import org.scalafmt.config.FormatEvent.CreateFormatOps
import org.scalafmt.config.FormatEvent.Enqueue
import org.scalafmt.config.FormatEvent.Explored
import org.scalafmt.config.FormatEvent.VisitToken
import org.scalafmt.config.ImportSelectors
import org.scalafmt.config.IndentOperator
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.internal.FormatWriter
import org.scalafmt.rewrite.Rewrite
import org.scalatest.FunSuiteLike

trait HasTests extends FunSuiteLike with FormatAssertions {
  import LoggerOps._
  import org.scalafmt.config.ScalafmtConfig._
  def scalafmtRunner(base: ScalafmtRunner): ScalafmtRunner = base.copy(
    debug = true,
    maxStateVisits = 150000,
    eventCallback = {
      case CreateFormatOps(ops) => Debug.formatOps = ops
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
  lazy val debugResults = mutable.ArrayBuilder.make[Result]
  val testDir =
    "scalafmt-tests/src/test/resources".replace("/", File.separator)

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

    val style: ScalafmtConfig = {
      val firstLine = split.head
      val pathPatternToReplace =
        if (OsSpecific.isWindows) s"""\\\\.*""" else "/.*"
      Try(
        spec2style(spec.replaceFirst(pathPatternToReplace, ""))
      ).getOrElse(
        Config.fromHoconString(firstLine.stripPrefix("ONLY ")) match {
          case Configured.Ok(c) => c
          case Configured.NotOk(c) =>
            throw new IllegalArgumentException(
              s"""Failed to parse filename $filename:
                 |$c""".stripMargin
            )
        }
      )
    }

    split.tail.map { t =>
      val before :: expected :: Nil = t.split(s"$sep>>>$sep", 2).toList
      val name :: original :: Nil = before.split(sep, 2).toList
      val actualName = stripPrefix(name)
      DiffTest(
        spec,
        actualName,
        filename,
        original,
        trimmed(expected),
        moduleSkip || isSkip(name),
        moduleOnly || isOnly(name),
        style
      )
    }
  }

  def spec2style(spec: String): ScalafmtConfig =
    spec match {
      case "unit" => ScalafmtConfig.unitTest40
      case "default" | "standard" | "scala" => ScalafmtConfig.unitTest80
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
      Debug.maxVisitedToken,
      visitedStates,
      Debug.elapsedNs
    )
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
                fail(
                  "test does not parse" +
                    parseException2Message(e, t.original)
                )
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
    val runner = scalafmtRunner(t.style.runner).copy(parser = parse)
    val obtained =
      Scalafmt.format(t.original, t.style.copy(runner = runner)).get
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
    val builder = mutable.ArrayBuilder.make[FormatOutput]()
    new FormatWriter(Debug.formatOps)
      .reconstructPath(Debug.tokens, Debug.state.splits, debug = onlyOne) {
        case (_, token, whitespace, _) =>
          builder += FormatOutput(
            token.left.syntax,
            whitespace,
            Debug.formatTokenExplored(token)
          )
      }
    builder.result()
  }

  private def trimmed(arg: String): String = arg.trim + "\n"
}
