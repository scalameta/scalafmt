package org.scalafmt.config

import scala.util.matching.Regex

import org.scalafmt.util.AbsoluteFile

case class FilterMatcher(include: Regex, exclude: Regex) {
  def matches(file: AbsoluteFile): Boolean = matches(file.path)
  def matches(input: String): Boolean =
    include.findFirstIn(input).isDefined &&
      exclude.findFirstIn(input).isEmpty
}

object FilterMatcher {
  val matchEverything = new FilterMatcher(".*".r, mkRegexp(Nil))

  def mkRegexp(filters: Seq[String], strict: Boolean = false): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ if strict => filters.mkString("^(", "|", ")$").r
      case _ => filters.mkString("(", "|", ")").r
    }

  def apply(includes: Seq[String], excludes: Seq[String]): FilterMatcher =
    new FilterMatcher(mkRegexp(includes), mkRegexp(excludes))
}
