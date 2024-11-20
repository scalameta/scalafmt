package org.scalafmt.community.common

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.CompatCollections.ParConverters._
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config._

import scala.meta._

import java.io._
import java.nio.file._

import munit.ComparisonFailException
import munit.diff.Diff
import munit.diff.console.AnsiColors

object TestHelpers {

  private[common] val communityProjectsDirectory = Paths
    .get("scalafmt-tests-community/target/community-projects")

  private val ignoreParts = List(
    ".git/",
    "tests/",
    "test/",
    "test-resources/scripting/",
    "test-resources/repl/",
    "sbt-test/",
    "out/",
  ).map(Paths.get(_))

  private def timeIt[A](block: => A): (Long, A) = {
    val t0 = System.currentTimeMillis()
    val res = block
    val t1 = System.currentTimeMillis()
    (t1 - t0, res)
  }

  private def runFile(styleName: String, path: Path, absPathString: String)(
      implicit style: ScalafmtConfig,
  ): TestStats = {
    def formatCode(code: String): (Long, Formatted.Result) =
      timeIt(Scalafmt.formatCode(code, style, filename = absPathString))
    val input = Input.File(path).chars
    val lines1 = input.count(_ == '\n')
    val (duration1, result1) = formatCode(new String(input))
    result1.formatted match {
      case x1: Formatted.Failure =>
        println(s"Failed for original file $absPathString")
        val trace = new StringWriter()
        trace.append(x1.e.getMessage).append('\n')
        x1.e.printStackTrace(new PrintWriter(trace))
        println(s"Error: " + trace.toString)
        TestStats(1, 1, Some(x1.e), duration1, lines1)
      case x1: Formatted.Success =>
        val stats1 = TestStats(1, 0, None, duration1, lines1)
        val out1 = x1.formattedCode
        val lines2 = x1.formattedCode.count(_ == '\n')
        val (duration2, result2) = formatCode(out1)
        def saveFormatted(): Unit = Files.writeString(
          Paths.get(absPathString + s".formatted.$styleName"),
          out1,
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING,
        )
        val stats2 = result2.formatted match {
          case x2: Formatted.Failure =>
            println(s"Failed for formatted file $absPathString")
            println(s"Error: " + x2.e.getMessage)
            saveFormatted()
            TestStats(1, 1, Some(x2.e), duration2, lines2)
          case x2: Formatted.Success =>
            val out2 = x2.formattedCode
            val diff = new Diff(out2, out1)
            if (diff.isEmpty) TestStats(1, 0, None, duration2, lines2)
            else {
              val msg = AnsiColors.filterAnsi(diff.createDiffOnlyReport())
              val loc = new munit.Location(absPathString, 0)
              val exc = new ComparisonFailException(msg, out2, out1, loc, false)
              println(s"Failed idempotency for file $absPathString")
              println(msg)
              saveFormatted()
              TestStats(1, 1, Some(exc), duration2, lines2)
            }
        }
        TestStats.merge(stats1, stats2)
    }
  }

  def checkFilesRecursive(styleName: String, path: Path)(implicit
      build: CommunityBuild,
      style: ScalafmtConfig,
  ): Option[TestStats] =
    if (ignoreParts.exists(path.endsWith)) None
    else {
      val ds = Files.newDirectoryStream(path)
      val (dirs, files) =
        try ds.iterator().asScala.toList.partition(Files.isDirectory(_))
        finally ds.close()
      val fileStats = files.compatPar.flatMap { x =>
        val fileStr = x.toString
        if (!fileStr.endsWith(".scala") || build.isExcluded(x)) None
        else Some(runFile(styleName, x, fileStr))
      }.reduceLeftOption(TestStats.merge)
      val dirStats = dirs.compatPar.flatMap(checkFilesRecursive(styleName, _))
        .reduceLeftOption(TestStats.merge)
      fileStats.fold(dirStats)(x =>
        dirStats.map(TestStats.merge(_, x)).orElse(fileStats),
      )
    }

}
