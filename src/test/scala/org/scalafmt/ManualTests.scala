package org.scalafmt

object ManualTests extends HasTests {

  val style = Standard

  val manual = ".manual"

  def tests: Seq[DiffTest] = {
    import FilesUtil._
    for {
      filename <- listFiles(testDir) if filename.endsWith(manual)
      test <- {
        val spec = filename
          .stripPrefix(testDir + "/")
          .stripSuffix(manual)
        readFile(filename).lines.map { name =>
          val original = readFile(stripPrefix(name))
          DiffTest(spec, stripPrefix(name), name, original, original,
          isSkip(name), isOnly(name), style)
        }
      }
    } yield test
  }
}

