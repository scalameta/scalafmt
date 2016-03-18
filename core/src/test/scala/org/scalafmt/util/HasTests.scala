package org.scalafmt.util

import java.io.File

import org.scalafmt.Error.UnknownStyle
import org.scalafmt.ScalaStyle

import scala.meta.Tree
import scala.meta.parsers.common.Parse

trait HasTests {
  val testDir = "core/src/test/resources"

  def tests: Seq[DiffTest]

  def testsToRun: Seq[DiffTest] = {
    val evalTests = tests
    val onlyTests = evalTests.filter(_.only)
    if (onlyTests.nonEmpty) onlyTests
    else tests
  }

  def isOnly(name: String) = name.startsWith("ONLY ")

  def isSkip(name: String) = name.startsWith("SKIP ")

  def stripPrefix(name: String) =
    name.stripPrefix("SKIP ").stripPrefix("ONLY ").trim

  def filename2parse(filename: String): Option[Parse[_ <: Tree]] =
    extension(filename) match {
      case "source" | "scala" | "scalafmt" =>
        Some(scala.meta.parsers.parseSource)
      case "stat" => Some(scala.meta.parsers.parseStat)
      case "case" => Some(scala.meta.parsers.parseCase)
      case _ => None
    }

  def extension(filename: String): String = filename.replaceAll(".*\\.", "")

  def parseDiffTests(content: String, filename: String): Seq[DiffTest] = {
    val spec = filename.stripPrefix(testDir + File.separator)
    val moduleOnly = isOnly(content)
    val moduleSkip = isSkip(content)

    content.split("\n<<< ").tail.map { t =>
      val before :: expected :: Nil = t.split("\n>>>\n", 2).toList
      val name :: original :: Nil = before.split("\n", 2).toList
      val actualName = stripPrefix(name)
      DiffTest(spec,
               actualName,
               filename,
               original,
               expected,
               moduleSkip || isSkip(name),
               moduleOnly || isOnly(name),
               file2style(filename))
    }
  }

  def file2style(filename: String): ScalaStyle =
    filename.split("/").reverse(1) match {
      case "unit" => ScalaStyle.UnitTest40
      case "default" | "standard" => ScalaStyle.UnitTest80
      case "scala" => ScalaStyle.UnitTest80
      case "scalajs" => ScalaStyle.ScalaJs
      case "stripMargin" => ScalaStyle.Default
      case style => throw UnknownStyle(style)
    }
}
