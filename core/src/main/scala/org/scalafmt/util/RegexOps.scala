package org.scalafmt.util

import scala.util.matching.Regex

object RegexOps {
  def mkRegexp(filters: Seq[String]): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ => filters.mkString("(", "|", ")").r
    }
}
