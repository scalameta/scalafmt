package org.scalafmt

import java.io.File

import org.scalactic.source.Position
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import org.scalatest.funsuite.AnyFunSuite

import org.scalafmt.Error.{Incomplete, SearchStateExploded}
import org.scalafmt.util._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.meta.Tree
import scala.meta.parsers.Parse
// TODO(olafur) property test: same solution without optimization or timeout.

class FormatTests
    extends AnyFunSuite
    with CanRunTests
    with FormatAssertions
    with DiffAssertions
    with BeforeAndAfterAllConfigMap {
  import LoggerOps._
  lazy val onlyUnit = UnitTests.tests.exists(_.only)
  lazy val onlyManual = !onlyUnit && ManualTests.tests.exists(_.only)
  lazy val onlyOne = tests.exists(_.only)

  override def ignore(t: DiffTest): Boolean = false

  override val tests = {
    if (onlyManual) ManualTests.tests
    else UnitTests.tests
  }

  tests
    .sortBy(x => (x.loc.fileName, x.loc.lineNumber))
    .withFilter(testShouldRun)
    .foreach(runTest(run))

  def run(t: DiffTest, parse: Parse[_ <: Tree]): Unit = {
    implicit val loc: Position = t.loc
    val debug = new Debug(onlyOne)
    val runner = t.style.runner.copy(parser = parse)
    val formatted = Scalafmt.formatCode(
      t.original,
      t.style.copy(runner = scalafmtRunner(runner, debug)),
      filename = loc.filePathname
    )
    debug.printTest()
    val obtained = formatted match {
      case Formatted.Failure(e)
          if t.style.onTestFailure.nonEmpty &&
            e.getMessage.contains(t.style.onTestFailure) =>
        t.expected
      case Formatted.Failure(e: Incomplete) => e.formattedCode
      case Formatted.Failure(e: SearchStateExploded) =>
        logger.elem(e)
        e.partialOutput
      case x => x.get
    }
    debugResults += saveResult(t, obtained, debug)
    if (t.style.rewrite.rules.isEmpty &&
      !t.style.assumeStandardLibraryStripMargin &&
      t.style.onTestFailure.isEmpty) {
      assertFormatPreservesAst(t.original, obtained)(
        parse,
        t.style.runner.dialect
      )
    }
    val formattedAgain = Scalafmt
      .formatCode(
        obtained,
        t.style.copy(runner = scalafmtRunner(runner, new Debug(false))),
        filename = loc.filePathname
      )
      .get
//          getFormatOutput(t.style, true) // uncomment to debug
    assertNoDiff(formattedAgain, obtained, "Idempotency violated")
    if (!onlyManual) {
      assertNoDiff(obtained, t.expected)
    }
  }

  def testShouldRun(t: DiffTest): Boolean = !onlyOne || t.only

  override def afterAll(configMap: ConfigMap): Unit = {
    logger.debug(s"Total explored: ${Debug.explored}")
    val results = debugResults.result()
    // TODO(olafur) don't block printing out test results.
    // I don't want to deal with scalaz's Tasks :'(
    val k = for {
      _ <- Future(
        FileOps.writeFile(
          s"target${File.separator}index.html",
          Report.heatmap(results)
        )
      )
    } yield ()
    // Travis exits right after running tests.
    if (sys.env.contains("TRAVIS")) Await.ready(k, 20.seconds)
  }
}
