package org.scalafmt

object ManualTests extends HasTests {
  val style = UnitTest80

  val manual = ".manual"

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
}
