package org.scalafmt

import org.scalafmt.util.DiffTest
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.HasTests

object ManualTests extends HasTests {

  lazy val tests: Seq[DiffTest] = {
    import FilesUtil._
    val manualFiles = for {
      filename <- listFiles(testDir)
      if filename.endsWith(manual)
      test <- {
        val spec = filename.stripPrefix(testDir + "/").stripSuffix(manual)
        readFile(filename).lines.map { name =>
          val original = readFile(stripPrefix(name))
          DiffTest(spec,
            stripPrefix(name),
            name,
            original,
            original,
            isSkip(name),
            isOnly(name),
            style)
        }
      }
    } yield test
    val scalaFiles = for {
      filename <- listFiles(testDir)
      if filename.endsWith(".scala")
    } yield {
      val content = readFile(filename)
      DiffTest(
        "Scala", filename, filename, content, content, false, false, style)
    }
    manualFiles ++ scalaFiles
  }
  val style = ScalaStyle.UnitTest80
  val manual = ".manual"
}
