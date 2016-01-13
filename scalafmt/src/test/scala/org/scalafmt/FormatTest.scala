package org.scalafmt

import scala.meta._
import org.scalatest.FunSuite
import org.scalafmt.DiffUtil._

case class Test(name: String, original: String, expected: String)

class FormatTest extends FunSuite with ScalaFmtLogger {

  val fmt = new ScalaFmt(Standard)

  def tests: Seq[Test] = {
    import FilesUtil._
    for {
      filename <- listFiles(
        "scalafmt/src/test/resources") if filename.endsWith(".test")
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

  tests.foreach {
    case Test(name, original, expected) =>
      test(name) {
        assert(fmt.format(original) diff expected)
      }
  }
}


