package org.scalafmt

import java.io.File

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffTest
import org.scalafmt.util.FileOps
import org.scalafmt.util.HasTests
import munit.Location

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
              testFile,
              new Location(testPath, 1),
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
        filename,
        new Location(filename, 1),
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
