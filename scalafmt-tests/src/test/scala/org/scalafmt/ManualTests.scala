package org.scalafmt

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffTest
import org.scalafmt.util.FileOps
import org.scalafmt.util.HasTests

object ManualTests extends HasTests {
  lazy val tests: Seq[DiffTest] = {
    import FileOps._
    val manualFiles = for {
      filename <- listFiles(testDir) if filename.endsWith(manual)
      test <- {
        val spec = filename.stripPrefix(testDir + "/").stripSuffix(manual)
        // Predef.augmentString = work around scala/bug#11125 on JDK 11
        augmentString(readFile(filename)).lines
          .withFilter(_.startsWith("ONLY"))
          .map { name =>
            val original = readFile(stripPrefix(name))
            DiffTest(
              spec,
              stripPrefix(name),
              name,
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
        "Scala",
        filename,
        filename,
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
