package org.scalafmt.config

import scala.util.matching.Regex

import org.scalafmt.sysops.AbsoluteFile

case class FilterMatcher(include: Regex, exclude: Regex) {
  def matchesFile(file: AbsoluteFile): Boolean = matches(file.toString())
  def matches(input: String): Boolean =
    include.pattern.matcher(input).find() &&
      !exclude.pattern.matcher(input).find()
}

object FilterMatcher {
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
