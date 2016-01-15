package org.scalafmt

import org.scalafmt.DiffUtil._
import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._


case class Test(name: String, original: String, expected: String)

class FormatTest extends FunSuite with Timeouts with ScalaFmtLogger {

  val fmt = new ScalaFmt(ScalaFmtTesting)

  def tests: Seq[Test] = {
    import FilesUtil._
    for {
      filename <- listFiles(
        "src/test/resources") if filename.endsWith(".test")
      test <- {
        val content = new String(
          java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(filename)))
        content.split("\n<<< ").tail.map { t =>
          val before :: expected :: Nil = t.split(">>>\n", 2).toList
          val name :: original :: Nil = before.split("\n", 2).toList
          Test(name, original, expected)
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
    case Test(name, original, expected) =>
      test(name) {
        failAfter(1 second) {
          assert(fmt.format(original) diff expected)
        }
      }
  }
}

