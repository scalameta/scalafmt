package org.scalafmt

object UnitTests extends HasTests with ScalaFmtLogger {

  override lazy val tests: Seq[DiffTest] = {
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
        val moduleOnly = isOnly(content)
        val moduleSkip = isSkip(content)

        content.split("\n<<< ").tail.map { t =>
          val before :: expected :: Nil = t.split(">>>\n", 2).toList
          val name :: original :: Nil = before.split("\n", 2).toList
          val actualName = stripPrefix(name)
          DiffTest(spec, actualName, filename,
            original, expected,
            moduleSkip || isSkip(name),
            moduleOnly || isOnly(name),
            file2style(filename)
          )
        }
      }
    } yield {
      test
    }
  }

  def file2style(filename: String): ScalaStyle =
    filename.split("/").reverse(1) match {
      case "unit" => UnitTestStyle
      case "standard" => Standard
      case _ =>
        logger.debug(s"Unknown dir $filename")
        ???
    }
}

