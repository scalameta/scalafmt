package org.scalafmt.config

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
    def apply(regex: String): Regex = {
      val pattern =
        try Pattern.compile(regex)
        catch {
          case e: Throwable => throw new ScalafmtConfigException(
              s"""|Illegal regex in configuration: $regex
                  |reason: ${e.getMessage}""".stripMargin,
            )
        }
      new Regex(pattern)
    }
  }

}
