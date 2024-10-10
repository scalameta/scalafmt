package org.scalafmt.util

import org.scalafmt.Debug
import org.scalafmt.Scalafmt
import org.scalafmt.config.ConfParsed
import org.scalafmt.config.DanglingParentheses
import org.scalafmt.config.FormatEvent._
import org.scalafmt.config.Indents
import org.scalafmt.config.NamedDialect
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtOptimizer
import org.scalafmt.config.ScalafmtParser
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.sysops.FileOps
import org.scalafmt.tests.BuildInfo

import java.nio.file.Paths
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.collection.mutable

import metaconfig.Conf
import metaconfig.Configured
import munit.Assertions._
import munit.Location

trait HasTests extends FormatAssertions {
  import HasTests._
  import LoggerOps._

  def scalafmtRunner(sr: ScalafmtRunner, dg: Debug): ScalafmtRunner = sr.copy(
    debug = true,
    maxStateVisits = sr.maxStateVisits.orElse(Some(150000)),
    completeCallback = dg.completed,
    eventCallback = {
      case CreateFormatOps(ops) => dg.formatOps = ops
      case Routes(routes) => dg.routes = routes
      case explored: Explored if explored.n % 10000 == 0 =>
        logger.elem(explored)
      case Enqueue(split) => dg.enqueued(split)
      case x: Written => dg.locations = x.formatLocations
      case _ =>
    },
  )

  lazy val debugResults = mutable.ArrayBuilder.make[Result]
  val testDir = BuildInfo.resourceDirectory.toPath

  def tests: Seq[DiffTest]

  def testsToRun: Seq[DiffTest] = {
    val evalTests = tests
    val onlyTests = evalTests.filter(_.only)
    if (onlyTests.nonEmpty) onlyTests else tests
  }

  def isOnly(name: String) = isPrefix(name, onlyPrefix)

  def isSkip(name: String) = isPrefix(name, skipPrefix)

  def stripPrefix(name: String) = stripPrefixOpt(name, skipPrefix)
    .orElse(stripPrefixOpt(name, onlyPrefix)).getOrElse(name)

  def filename2parse(filename: String): Option[ScalafmtParser] =
    extension(filename) match {
      case "source" | "scala" | "scalafmt" => Some(ScalafmtParser.Source)
      case "stat" => Some(ScalafmtParser.Stat)
      case "case" => Some(ScalafmtParser.Case)
      case _ => None
    }

  def extension(filename: String): String = filename.replaceAll(".*\\.", "")

  def parseDiffTests(filename: String): Seq[DiffTest] = {
    val content = FileOps.readFile(filename)
    val sep =
      if (content.contains(System.lineSeparator)) System.lineSeparator else "\n"
    val spec = BuildInfo.resourceDirectory.toPath
      .relativize(Paths.get(filename)).getName(0).toString

    val split = content.split(s"(?:^|$sep)<<< ")
    if (split.length <= 1) return Seq.empty // RETURNING!!!

    val (head, tail) = (split.head, split.tail)
    val moduleOnly = isOnly(head)
    val moduleSkip = isSkip(head)

    def loadStyle(cfg: String, base: ScalafmtConfig, ln: Int): ScalafmtConfig =
      ScalafmtConfig.fromHoconString(cfg, base).getOrRecover { c =>
        throw new IllegalArgumentException(
          s"""|Failed to parse line=$ln filename $filename:
              |$cfg
              |$c""".stripMargin,
        )
      }
    val style: ScalafmtConfig = loadStyle(
      stripPrefixOpt(head, onlyPrefix).getOrElse(head), {
        val base = spec2style(spec)
        filename2parse(filename).fold(base) { x =>
          base.copy(runner = base.runner.withParser(x))
        }
      },
      1,
    )

    @tailrec
    def numLines(str: String, cnt: Int = 1, off: Int = 0): Int = {
      val idx = str.indexOf(sep, off)
      if (idx < 0) cnt else numLines(str, cnt + 1, idx + sep.length)
    }
    var linenum = numLines(head, 2)
    val inputOutputRegex = Pattern.compile(
      s"(.+?)$sep(?:(.+)$sep===$sep)?(.+)$sep>>>(?: +(.+?))?$sep(.*)",
      Pattern.DOTALL,
    )
    tail.map { t =>
      val matcher = inputOutputRegex.matcher(t)
      if (!matcher.matches())
        throw new IllegalStateException(s"invalid test, missing delimiters:\n$t")
      val name = matcher.group(1)
      val extraConfig = Option(matcher.group(2))
      val original = matcher.group(3)
      val extra = Option(matcher.group(4)).flatMap { x =>
        ConfParsed.fromString(x).conf match {
          case Configured.Ok(v) => Some(v)
          case _ => Some(Conf.Str(x))
        }
      }
      val expected = matcher.group(5)
      val testStyle = extraConfig.fold(style)(loadStyle(_, style, linenum))

      val altFilename = extra match {
        case Some(Conf.Str(value)) => Some(value)
        case Some(x: Conf.Obj) => x.field("filename") match {
            case Some(Conf.Str(value)) => Some(value)
            case _ => None
          }
        case _ => None
      }
      def getExtraNum(field: String) = extra match {
        case Some(x: Conf.Obj) => x.field(field) match {
            case Some(Conf.Num(value)) => Some(value.intValue)
            case _ => None
          }
        case _ => None
      }

      val actualName = stripPrefix(name)
      val test = DiffTest(
        actualName,
        altFilename.getOrElse(filename),
        new Location(filename, linenum),
        original,
        trimmed(expected),
        moduleSkip || isSkip(name),
        moduleOnly || isOnly(name),
        testStyle,
        stateVisits = getExtraNum("stateVisits"),
        stateVisits2 = getExtraNum("stateVisits2"),
      )
      linenum += numLines(t)
      test
    }
  }

  private def spec2style(spec: String): ScalafmtConfig = spec match {
    case "unit" => unitTest40
    case "default" | "standard" | "scala" => unitTest80
    case "scalajs" => scalaJsConfig
    case "scala3" => scala3Config
    case _ => defaultConfig
  }

  def saveResult(t: DiffTest, obtained: String, debug: Debug): Result = {
    val output = getFormatOutput(debug)
    val obtainedHtml = Report.mkHtml(output)
    Result(
      t,
      obtained,
      obtainedHtml,
      output,
      debug.formatTokenExplored.fold(0)(_.max),
      debug.explored.getOrElse(0),
      debug.elapsedNs,
    )
  }

  def ignore(t: DiffTest): Boolean = false

  def defaultRun(t: DiffTest)(implicit loc: Location): Unit = {
    val debug = new Debug(false)
    val runner = scalafmtRunner(t.style.runner, debug)
    val result = Scalafmt.formatCode(
      t.original,
      t.style.copy(runner = runner),
      filename = t.filename,
    )
    val obtained = result.formatted.get
    if (t.style.rewrite.rules.isEmpty) assertFormatPreservesAst(
      t.filename,
      t.original,
      obtained,
      result.config.runner,
    )
    assertNoDiff(obtained, t.expected)
  }

  def getFormatOutput(debug: Debug): Array[FormatOutput] = {
    val builder = mutable.ArrayBuilder.make[FormatOutput]
    Option(debug.locations).foreach(_.foreach { entry =>
      val token = entry.curr.formatToken
      implicit val sb = new StringBuilder()
      sb.append(token.left.syntax)
      entry.formatWhitespace(0)
      builder += FormatOutput(
        sb.result(),
        debug.formatTokenExplored.fold(-1)(_(token.meta.idx)),
      )
    })
    builder.result()
  }

  private def trimmed(arg: String): String = arg.trim + "\n"
}

object HasTests {

  private val defaultDialect = NamedDialect("scala213", NamedDialect.scala213)

  private def withoutSlowStates(cfg: ScalafmtConfig): ScalafmtConfig = cfg
    .copy(runner =
      cfg.runner.copy(optimizer =
        cfg.runner.optimizer
          .copy(pruneSlowStates = ScalafmtOptimizer.PruneSlowStates.Only),
      ),
    )

  private val defaultConfig =
    withoutSlowStates(ScalafmtConfig.default.withDialect(defaultDialect))
  private val scala3Config = defaultConfig
    .withDialect(NamedDialect.scala3, "scala3")
  private val scalaJsConfig = defaultConfig.forScalaJs.copy(maxColumn = 79)

  private val testing = defaultConfig.copy(
    maxColumn = 79,
    assumeStandardLibraryStripMargin = false,
    includeCurlyBraceInSelectChains = false,
    danglingParentheses = DanglingParentheses(false, false),
    align = defaultConfig.align.copy(
      tokens = Seq.empty,
      openParenCallSite = true,
      openParenDefnSite = true,
    ),
    optIn = ScalafmtConfig.default.optIn
      .copy(breakChainOnFirstMethodDot = false),
    // The new aggressive config style breaks ~40 unit tests. The diff output
    // looks nice, but updating the unit tests would take too much time.
    // I can imagine that I will throw away most of the tests and replace them
    // with autogenerated tests from scala-repos.
    runner = defaultConfig.runner.conservative,
  )

  val unitTest80 = testing.copy(indent = Indents(callSite = 4, defnSite = 4))

  val unitTest40 = unitTest80.copy(maxColumn = 39)

  private[scalafmt] val onlyPrefix = "ONLY"
  private[scalafmt] val skipPrefix = "SKIP"

  private def isPrefix(name: String, prefix: String) = name
    .startsWith(prefix) && !name.charAt(prefix.length).isLetterOrDigit

  private def stripPrefixOpt(name: String, prefix: String) =
    if (isPrefix(name, prefix)) Some(name.substring(prefix.length).trim)
    else None

}
