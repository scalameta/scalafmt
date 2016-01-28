package org.scalafmt

object UnitTests extends HasTests with ScalaFmtLogger {

  def file2style(filename: String): ScalaStyle = filename.split("/").reverse(1) match {
    case "unit" => UnitTestStyle
    case "standard" => Standard
    case _ =>
      logger.debug(s"Unknown dir $filename")
      ???
  }

  override def tests: Seq[DiffTest] = {
    import FilesUtil._
    for {
      filename <- listFiles(testDir) if filename2parse(filename).isDefined
      test <- {
         // Tests are sorted first by spec, and warmup should run first.
        val spec =
          if (filename.contains("Warmup"))
            "===> Warmup"
          else
            filename.stripPrefix(testDir + "/").replaceAll("\\.[^\\.]*$", "")

        val content = readFile(filename)
        val prefix = if (content.startsWith("ONLY")) "ONLY " else ""
        if (content.startsWith("SKIP")) Array.empty[DiffTest]
        else {
          content.split("\n<<< ").tail.map { t =>
            val before :: expected :: Nil = t.split(">>>\n", 2).toList
            val name :: original :: Nil = before.split("\n", 2).toList
            DiffTest(spec, prefix + name, filename,
              original, expected, file2style(filename))
          }
        }
      }
    } yield {
      test
    }
  }
}

