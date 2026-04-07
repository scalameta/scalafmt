package org.scalafmt

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.sysops._
import org.scalafmt.util.FormatAssertions

import scala.meta.dialects.Scala213

import java.io.File
import java.nio.file.Path

import munit.FunSuite

/** Asserts formatter does not alter original source file's AST.
  *
  * Will maybe use scalacheck someday.
  */
class FidelityTest extends FunSuite with FormatAssertions {

  private val numFiles = 277

  private val denyList = Set(
    "ConfigReader.scala",
    "BuildTime.scala",
    "GitCommit.scala",
    "/target/",
    "/resources/",
    "/gh-pages/",
  ).map(_.replace("/", File.separator))

  override def munitTests(): Seq[Test] = {
    var cnt = 0
    val visitor = new FileOps.WalkVisitor {
      override def onTree(dir: Path, fileStat: FileStat): FileOps.WalkVisit = {
        val name = dir.getFileName.toString
        val skip = name.length > 1 && (name(0) == '.' || name == "target")
        if (skip) FileOps.WalkVisit.Skip else FileOps.WalkVisit.Good
      }
      override def onFile(file: Path, fileStat: FileStat): FileOps.WalkVisit = {
        if (fileStat.isRegularFile && testForFidelity(file)) cnt += 1
        FileOps.WalkVisit.Good
      }
      override def onFailStop(file: Path, exc: Throwable): Boolean = {
        test(s"init failure: $file")(
          fail(s"Failed to init test: ${exc.getMessage}", exc),
        )
        false
      }
    }
    FileOps.walkFiles(visitor)(FileOps.getPath("."))

    test("count of files")(
      assertEquals(cnt, numFiles, s"Expected $numFiles files to test, got $cnt"),
    )

    super.munitTests()
  }

  private def testForFidelity(path: Path): Boolean = path.getFileName.toString
    .endsWith(".scala") && {
    val filename = path.toString
    val ok = !denyList.exists(filename.contains)
    if (ok) test(filename)(
      PlatformFileOps.readFileAsync(path).map { code =>
        val formatted = Scalafmt
          .formatCode(code, ScalafmtConfig.default, filename = filename)
        assertFormatPreservesAst(filename, code, formatted.get)(
          scala.meta.parsers.Parse.parseSource,
          Scala213,
        )
      }(munitExecutionContext),
    )
    ok
  }

}
