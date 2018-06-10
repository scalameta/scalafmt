package org.scalafmt

import org.scalafmt.util.DiffTest
import org.scalafmt.util.FileOps
import org.scalafmt.util.HasTests

object UnitTests extends HasTests {
  import FileOps._

  /** Avoids parsing all files if some tests are marked ONLY.
    */
  def getTestFiles: Vector[String] = {
    val testsFiles = listFiles(testDir).filter(filename2parse(_).isDefined)
    val onlyTests = testsFiles.filter(_.contains("\n<<< ONLY"))
    if (onlyTests.nonEmpty) onlyTests
    else testsFiles
  }

  // TODO(olafur) make possible to limit states per unit test.
  override lazy val tests: Seq[DiffTest] = {
    for {
      filename <- getTestFiles
      test <- {
        val content = readFile(filename)
        parseDiffTests(content, filename)
      }
    } yield {
      if (sys.env.contains("CI") && test.only) {
        sys.error(
          s"""Please remove ONLY from test '${test.name}' in file '$filename'.
             |Tests with ONLY will not be merged, this feature is only meant to be used for local development.
           """.stripMargin
        )
      }
      test
    }
  }
}
