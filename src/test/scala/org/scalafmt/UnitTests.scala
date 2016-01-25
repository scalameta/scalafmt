package org.scalafmt

class UnitTests extends FormatTest {

  override def tests: Seq[DiffTest] = {
    import FilesUtil._
    var isOnly = false
    for {
      filename <- listFiles(testDir) if filename.endsWith(".test")
      test <- {
        val spec = filename.stripPrefix(testDir + "/").stripSuffix(".test")
        val content = readFile(filename)
        val prefix = if (content.startsWith("ONLY")) "ONLY " else ""

        if (content.startsWith("SKIP")) Array.empty[DiffTest]
        else {
          content.split("\n<<< ").tail.map { t =>
            val before :: expected :: Nil = t.split(">>>\n", 2).toList
            val name :: original :: Nil = before.split("\n", 2).toList
            DiffTest(spec, prefix + name, original, expected)
          }
        }
      }
    } yield {
      test
    }
  }
}

