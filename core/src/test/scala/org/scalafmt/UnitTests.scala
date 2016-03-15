package org.scalafmt

import java.io.File

import org.scalafmt.Error.UnknownStyle
import org.scalafmt.internal.ScalaFmtLogger
import org.scalafmt.util.DiffTest
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.HasTests

object UnitTests extends HasTests with ScalaFmtLogger {
  import FilesUtil._

  /** Avoids parsing all files if some tests are marked ONLY.
    */
  def getTestFiles: Vector[String] = {
    val testsFiles = listFiles(testDir).filter(filename2parse(_).isDefined)
    val onlyTests = testsFiles.filter(_.contains("\n<<< ONLY"))
    if (onlyTests.nonEmpty) onlyTests
    else testsFiles
  }

  // TODO(olafur) make possible to limit states per unit test.
  override lazy val tests: Seq[DiffTest] = {
    for {
      filename <- getTestFiles
      test <- {
        val spec = filename.stripPrefix(testDir + File.separator)
        val content = readFile(filename)
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
    } yield {
      test
    }
  }

  def file2style(filename: String): ScalaStyle =
    filename.split("/").reverse(1) match {
      case "unit" => ScalaStyle.UnitTest40
      case "default" | "standard" => ScalaStyle.UnitTest80
      case "scala" => ScalaStyle.UnitTest80
      case "scalajs" => ScalaStyle.ScalaJs
      case style => throw UnknownStyle(style)
    }
}
