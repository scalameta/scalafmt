package org.scalafmt.community.common

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config._

import scala.meta._

import java.io._
import java.nio.file._

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import munit.ComparisonFailException
import munit.diff.Diff
import munit.diff.console.AnsiColors

object TestHelpers {

  implicit val executionContext: ExecutionContext = ExecutionContext.global

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
        val error = new StringWriter()
        error.append(s"Failed for original file $absPathString\n")
        error.append("Error: ").append(x1.e.getMessage).append('\n')
        x1.e.printStackTrace(new PrintWriter(error))
        println(error.toString)
        TestStats(1, 1, Some(x1.e), duration1, lines1)
      case x1: Formatted.Success =>
        val stats1 = TestStats(1, 0, None, duration1, lines1)
        val out1 = x1.formattedCode
        val lines2 = x1.formattedCode.count(_ == '\n')
        val (duration2, result2) = formatCode(out1)
        def saveFormatted(): Unit = Files.write(
          Paths.get(absPathString + s".formatted.$styleName"),
          out1.getBytes(),
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING,
        )
        val stats2 = result2.formatted match {
          case x2: Formatted.Failure =>
            print(
              s"""|Failed for formatted file $absPathString
                  |Error: ${x2.e.getMessage}
                  |""".stripMargin,
            )
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
              print(
                s"""|Failed idempotency for file $absPathString
                    |$msg
                    |""".stripMargin,
              )
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
      futures: mutable.Growable[Future[TestStats]],
  ): Unit = if (!ignoreParts.exists(path.endsWith)) {
    val ds = Files.newDirectoryStream(path)
    val (dirs, files) =
      try ds.iterator().asScala.toList.partition(Files.isDirectory(_))
      finally ds.close()
    files.foreach { x =>
      val fileStr = x.toString
      if (fileStr.endsWith(".scala") && !build.isExcluded(x)) futures +=
        Future(runFile(styleName, x, fileStr))
    }
    dirs.foreach(checkFilesRecursive(styleName, _))
  }

}
