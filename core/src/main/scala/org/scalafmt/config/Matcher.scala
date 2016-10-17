package org.scalafmt.config

import scala.util.matching.Regex

class Matcher(include: Regex, exclude: Regex) {
  def matches(input: String): Boolean = {
    include.findFirstIn(input).isDefined &&
    exclude.findFirstIn(input).isEmpty
  }
}
