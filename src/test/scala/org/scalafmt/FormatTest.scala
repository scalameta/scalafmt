package org.scalafmt

import org.scalafmt.DiffUtil._
import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._


case class Test(spec: String, name: String, original: String, expected: String)

class FormatTest extends FunSuite with Timeouts with ScalaFmtLogger {

  val fmt = new ScalaFmt(ScalaFmtTesting)
  val testDir = "src/test/resources"

  def tests: Seq[Test] = {
    import FilesUtil._
    for {
      filename <- listFiles(testDir) if filename.endsWith(".test")
      test <- {
        val spec = filename.stripPrefix(testDir + "/").stripSuffix(".test")
        val content = new String(
          java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(filename)))
        content.split("\n<<< ").tail.map { t =>
          val before :: expected :: Nil = t.split(">>>\n", 2).toList
          val name :: original :: Nil = before.split("\n", 2).toList
          Test(spec, name, original, expected)
        }
      }
    } yield {
      test
    }
  }

  val onlyOne = tests.exists(_.name.startsWith("ONLY"))

  tests.withFilter { t =>
    !t.name.startsWith("SKIP") &&
      (!onlyOne || t.name.startsWith("ONLY"))
  }.foreach {
    case Test(spec, name, original, expected) =>
      val testName = s"$name"
      test(f"$testName%-39s|") {
        failAfter(1 second) {
          assert(fmt.format(original) diff expected)
        }
      }
  }
}

