package org.scalafmt

object ManualTests extends HasTests {

  val style = Standard

  val manual = ".manual"

  def stripFilename(filename: String) = filename
    .stripPrefix("SKIP")
    .stripPrefix("ONLY")
    .trim

  def tests: Seq[DiffTest] = {
    import FilesUtil._
    for {
      filename <- listFiles(testDir) if filename.endsWith(manual)
      test <- {
        val spec = filename
          .stripPrefix(testDir + "/")
          .stripSuffix(manual)
        readFile(filename).lines.map { name =>
          val original = readFile(stripFilename(name))
          DiffTest(spec, name, name, original, original, style)
        }
      }
    } yield test
  }
}

