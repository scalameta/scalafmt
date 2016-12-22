package org.scalafmt.util

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.parsers.Parse
import scala.meta.parsers.ParseException
import scala.util.Try
import java.io.File

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
  val testDir = "core/src/test/resources".replace("/", File.separator)

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
      if (content.contains(OsSpecific.lineSeparator)) OsSpecific.lineSeparator
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
        Config.fromHocon(firstLine.stripPrefix("ONLY ")) match {
          case Right(s)
              if !firstLine.replaceAll("\\s+", "").isEmpty &&
                !firstLine.startsWith("//") =>
            s
          case Left(e) => throw UnableToFindStyle(spec, e)
        }
      )
    }

    split.tail.map { t =>
      val before :: expected :: Nil = t.split(s"$sep>>>$sep", 2).toList
      val name :: original :: Nil = before.split(sep, 2).toList
      val actualName = stripPrefix(name)
      DiffTest(spec,
               actualName,
               filename,
               original,
               expected,
               moduleSkip || isSkip(name),
               moduleOnly || isOnly(name),
               style)
    }
  }

  def spec2style(spec: String): ScalafmtConfig =
    spec match {
      case "unit" => ScalafmtConfig.unitTest40
      case "default" | "standard" | "scala" =>
        ScalafmtConfig.unitTest80
      case "default140" => ScalafmtConfig.unitTest80.copy(maxColumn = 140)
      case "default100" => ScalafmtConfig.unitTest80.copy(maxColumn = 100)
      case "scalajs" => ScalafmtConfig.scalaJs
      case "intellij" => ScalafmtConfig.intellij
      case "noAlign" =>
        ScalafmtConfig.unitTest80.copy(
          maxColumn = 40,
          align = unitTest80.align.copy(
            openParenCallSite = false
          )
        )
      case "stripMargin" =>
        ScalafmtConfig.unitTest80.copy(assumeStandardLibraryStripMargin = true)
      case "spaces" =>
        ScalafmtConfig.unitTest80.copy(
          spaces = unitTest80.spaces
            .copy(inImportCurlyBraces = true, afterTripleEquals = true)
        )
      case "align" => ScalafmtConfig.addAlign(ScalafmtConfig.unitTest80)
      case "alignNoSpace" =>
        ScalafmtConfig.unitTest80.copy(
          align = ScalafmtConfig.unitTest80.align.copy(
            tokens = Set(
              AlignToken(":", ".*"),
              AlignToken(",", ".*")
            )
          )
        )
      case "parentConstructors" =>
        ScalafmtConfig.unitTest80.copy(
          binPack = BinPack(false, false, parentConstructors = true),
          maxColumn = 40
        )
      case "alignByArrowEnumeratorGenerator" =>
        ScalafmtConfig.unitTest40.copy(
          align = ScalafmtConfig.unitTest40.align
            .copy(arrowEnumeratorGenerator = true)
        )
      case "noIndentOperators" =>
        ScalafmtConfig.unitTest80.copy(unindentTopLevelOperators = true,
                                       indentOperator = IndentOperator.akka)
      case "unicode" =>
        ScalafmtConfig.unitTest80.copy(
          rewriteTokens = Map(
            "=>" -> "⇒",
            "<-" -> "←"
          )
        )
      case "spacesBeforeContextBound" =>
        ScalafmtConfig.unitTest80.copy(
          spaces = unitTest80.spaces.copy(beforeContextBoundColon = true))
      case "trailing-commas" =>
        ScalafmtConfig.unitTest40.copy(
          poorMansTrailingCommasInConfigStyle = true)
      case "import" =>
        ScalafmtConfig.unitTest80.copy(
          importSelectors = ImportSelectors.noBinPack)
      case "importSingleLine" =>
        ScalafmtConfig.unitTest80.copy(
          importSelectors = ImportSelectors.singleLine)
      case "keepLineBreaks" =>
        ScalafmtConfig.unitTest80.copy(
          optIn = unitTest80.optIn.copy(
            breakChainOnFirstMethodDot = true
          ))
      case "newlineBeforeLambdaParams" =>
        ScalafmtConfig.default.copy(
          newlines = default.newlines.copy(
            alwaysBeforeCurlyBraceLambdaParams = true
          )
        )
      case x if Rewrite.name2rewrite.contains(x) =>
        ScalafmtConfig.default.copy(
          rewrite = default.rewrite.copy(rules = Seq(Rewrite.name2rewrite(x)))
        )
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
                fail(
                  "test does not parse" +
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
    val runner = scalafmtRunner(t.style.runner).copy(parser = parse)
    val obtained =
      Scalafmt.format(t.original, t.style.copy(runner = runner)).get
    if (t.style.rewrite.rules.isEmpty) {
      assertFormatPreservesAst(t.original, obtained)(parse)
    }
    assertNoDiff(obtained, t.expected)
  }

  def getFormatOutput(style: ScalafmtConfig,
                      onlyOne: Boolean): Array[FormatOutput] = {
    val builder = mutable.ArrayBuilder.make[FormatOutput]()
    new FormatWriter(Debug.formatOps)
      .reconstructPath(Debug.tokens, Debug.state.splits, debug = onlyOne) {
        case (_, token, whitespace) =>
          builder += FormatOutput(token.left.syntax,
                                  whitespace,
                                  Debug.formatTokenExplored(token))
      }
    builder.result()
  }
}
