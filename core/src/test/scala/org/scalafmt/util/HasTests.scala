package org.scalafmt.util

import scala.meta.Tree
import scala.meta.parsers.common.Parse

trait HasTests {
  val testDir = "core/src/test/resources"

  def isOnly(name: String) = name.startsWith("ONLY ")

  def isSkip(name: String) = name.startsWith("SKIP ")

  def stripPrefix(name: String) =
    name.stripPrefix("SKIP ").stripPrefix("ONLY ").trim

  def filename2parse(filename: String): Option[Parse[_ <: Tree]] =
    extension(filename) match {
      case "source" | "scala" | "scalafmt" => Some(scala.meta.parsers.parseSource)
      case "stat" => Some(scala.meta.parsers.parseStat)
      case "case" => Some(scala.meta.parsers.parseCase)
      case _ => None
    }

  def extension(filename: String): String = filename.replaceAll(".*\\.", "")

  def tests: Seq[DiffTest]
}
