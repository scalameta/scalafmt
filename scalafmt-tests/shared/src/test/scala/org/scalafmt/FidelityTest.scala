package org.scalafmt

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.sysops.FileOps
import org.scalafmt.sysops.PlatformFileOps
import org.scalafmt.util.FormatAssertions

import scala.meta.dialects.Scala213

import java.io.File

import munit.FunSuite

/** Asserts formatter does not alter original source file's AST.
  *
  * Will maybe use scalacheck someday.
  */
class FidelityTest extends FunSuite with FormatAssertions {

  private val denyList = Set(
    "ConfigReader.scala",
    "BuildTime.scala",
    "GitCommit.scala",
    "/target/",
    "/resources/",
    "/gh-pages/",
  ).map(_.replace("/", File.separator))

  FileOps.listFiles(".").foreach { x =>
    val filename = x.toString
    val ok = filename.endsWith(".scala") && !denyList.exists(filename.contains)
    if (ok) test(filename)(
      PlatformFileOps.readFileAsync(x).map { code =>
        val formatted = Scalafmt
          .formatCode(code, ScalafmtConfig.default, filename = filename)
        assertFormatPreservesAst(filename, code, formatted.get)(
          scala.meta.parsers.Parse.parseSource,
          Scala213,
        )
      }(munitExecutionContext),
    )
  }

}
