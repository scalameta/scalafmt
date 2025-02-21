package org.scalafmt.sysops

import java.nio.file.Path
import java.util.regex.Pattern

trait PathMatcher {
  def matches(path: Path): Boolean
}

object PathMatcher {

  class Regex(pattern: Pattern) extends PathMatcher {
    def matches(path: Path): Boolean = pattern.matcher(path.toString).find()
  }

  object Regex {
    def apply(regex: String): Regex =
      try new Regex(Pattern.compile(regex))
      catch {
        case e: Throwable =>
          val msg = s"Invalid path patcher regex: /$regex/; ${e.getMessage}"
          throw new ScalafmtSysException(msg)
      }

  }

}
