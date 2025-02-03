package org.scalafmt

import org.scalafmt.sysops.FileOps
import org.scalafmt.util.DiffTest
import org.scalafmt.util.HasTests

import java.nio.file.Path

object UnitTests extends HasTests {
  import FileOps._

  /** Avoids parsing all files if some tests are marked ONLY.
    */
  def getTestFiles: Seq[String] = {
    val testsFiles = listFiles(testDir).map(_.toString)
      .filter(filename2parse(_).isDefined)
    val onlyTests = testsFiles.filter(_.contains("\n<<< ONLY"))
    if (onlyTests.nonEmpty) onlyTests else testsFiles
  }

  // TODO(olafur) make possible to limit states per unit test.
  override lazy val tests: Seq[DiffTest] = {
    def checkPath(p: Path) = filename2parse(p.toString).isDefined
    for {
      filename <- listFiles(testDir, (p, a) => checkPath(p) && a.isRegularFile)
      test <- parseDiffTests(filename, notOnly = sys.env.contains("CI"))
    } yield test
  }
}
