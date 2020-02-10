package org.scalafmt

import java.io.File

import org.scalactic.source.Position
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffTest
import org.scalafmt.util.FileOps
import org.scalafmt.util.HasTests

object ManualTests extends HasTests {
  lazy val tests: Seq[DiffTest] = {
    import FileOps._
    val testPrefix = testDir + File.separator
    val manualFiles = for {
      filename <- listFiles(testDir) if filename.endsWith(manual)
      test <- {
        readFile(filename).linesIterator
          .withFilter(_.startsWith("ONLY"))
          .map { name =>
            val testPath = stripPrefix(name)
            val original = readFile(testPath)
            val testFile = testPath.stripPrefix(testPrefix)
            DiffTest(
              testFile,
              new Position(testFile, testPath, 1),
              original,
              original,
              isSkip(name),
              isOnly(name),
              style
            )
          }
      }
    } yield test
    val scalaFiles = for {
      filename <- listFiles(testDir) if filename.endsWith(".scala")
    } yield {
      val content = readFile(filename)
      DiffTest(
        filename,
        new Position(filename, filename, 1),
        content,
        content,
        false,
        false,
        style
      )
    }
    manualFiles ++ scalaFiles
  }
  val style = ScalafmtConfig.default
  val manual = ".manual"
}
