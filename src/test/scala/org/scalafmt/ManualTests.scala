package org.scalafmt

class ManualTests extends FormatTest {

  override def style = Standard

  def stripFilename(filename: String) = filename
    .stripPrefix("SKIP")
    .stripPrefix("ONLY")
    .trim

  override def tests: Seq[DiffTest] = {
    import FilesUtil._
    for {
      filename <- listFiles(testDir) if filename.endsWith(".manual")
      test <- {
        val spec = filename
          .stripPrefix(testDir + "/")
          .stripSuffix(".manual")
        readFile(filename).lines.map { name =>
          val original = readFile(stripFilename(name))
          DiffTest(spec, name, original, original)
        }
      }
    } yield test
  }
}

