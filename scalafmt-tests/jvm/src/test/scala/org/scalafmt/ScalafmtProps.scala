package org.scalafmt

import org.scalafmt.CompatCollections.ParConverters._
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.util.FormatAssertions

import org.scalameta.logger
import scala.meta._
import scala.meta.testkit._

import scala.collection.mutable

import munit.FailException
import munit.FunSuite

class ScalafmtProps extends FunSuite with FormatAssertions {
  import ScalafmtProps._
  def getBugs(
      config: ScalafmtConfig = ScalafmtConfig.default,
      count: Int = Int.MaxValue,
  ): mutable.Seq[(CorpusFile, Observation[Bug])] = {
    val corpus = Corpus.files(
      // TODO(olafur) remove once testkit 1.7 is out
      Corpus.fastparse.copy(Corpus.fastparse.url.replace("olafurpg", "scalameta")),
    ).take(count).toBuffer.compatPar
    SyntaxAnalysis.run[Observation[Bug]](corpus) { file =>
      val code = file.read
      try Scalafmt.format(code, config) match {
          case Formatted.Success(formatted) =>
            assertFormatPreservesAst[Source](file.filename, code, formatted)
            val formattedSecondTime = Scalafmt.format(formatted, config).get
            try assertNoDiff(formattedSecondTime, formatted, "Idempotence")
            catch {
              case diff: FailException => throw DiffFailure(
                  "Idempotence",
                  formatted,
                  formattedSecondTime,
                  diff.getMessage,
                )
            }
            Nil
          case Formatted.Failure(_: ParseException | _: TokenizeException) =>
            Nil
          case Formatted.Failure(e: Error.SearchStateExploded) =>
            List(Observation("", e.line, SearchStateExploded))
          case Formatted.Failure(e) =>
            List(Observation(e.getMessage, 0, Unknown(e)))
        }
      catch {
        case e: Error.FormatterChangedAST =>
          List(Observation(e.getMessage, -1, AstChanged))
        case e: Error.FormatterOutputDoesNotParse => List(Observation(
            e.getMessage.linesIterator.slice(1, 2).mkString(""),
            e.line,
            FormattedOutputDoesNotParse,
          ))
        case e: Error => List(Observation(e.getMessage, -1, Unknown(e)))
        case e: DiffFailure =>
          val line = e.obtained.linesIterator.zip(e.expected.linesIterator)
            .takeWhile { case (a, b) => a == b }.length
          List(Observation(
            e.diff.linesIterator.take(3).mkString("\n"),
            line,
            NonIdempotent,
          ))
      }
    }
  }

  def printReport(bugs: List[(CorpusFile, Observation[Bug])]): Unit = {
    val table = Observation.markdownTable(bugs)
    val summary = bugs.groupBy(_._2.kind.toString).mapValues(_.length).toSeq
      .sortBy(_._2).map { case (a, b) => s"$a=$b" }.mkString("\n")
    val report = s"""|$summary
                     |
                     |$table """.stripMargin
    logger.elem(summary)
    logger.elem(report)
    logger.elem(summary)
    (AbsoluteFile.userDir / "target" / "scalafmt-props.md").writeFile(report)
  }
}

object ScalafmtProps {
  sealed trait Bug
  case object NonIdempotent extends Bug
  case object AstChanged extends Bug
  case object FormattedOutputDoesNotParse extends Bug
  case object SearchStateExploded extends Bug
  case class Unknown(e: Throwable) extends Bug

  case class DiffFailure(
      title: String,
      expected: String,
      obtained: String,
      diff: String,
  ) extends Exception

  def main(args: Array[String]): Unit = {
    val props = new ScalafmtProps()
    val bugs = props.getBugs()
    val idempotency = bugs.count(_._2.kind == NonIdempotent)
    val searchState = bugs.count(_._2.kind == SearchStateExploded)
    val badOutput = bugs.count(_._2.kind == FormattedOutputDoesNotParse)
    assert(idempotency <= 41)
    assert(searchState <= 34)
    assert(badOutput <= 4)
  }
}
