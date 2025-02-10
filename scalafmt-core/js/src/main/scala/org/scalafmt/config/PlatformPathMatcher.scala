package org.scalafmt.config

object PlatformPathMatcher {
  private def fail(pattern: String, msg: String): Nothing =
    throw new ScalafmtConfigException(
      s"Illegal pattern in configuration: $pattern; $msg",
    )
  def apply(pattern: String): PathMatcher = {
    val colon = pattern.indexOf(':')
    if (colon <= 0) fail(pattern, s"no path matcher prefix")
    pattern.substring(0, colon) match {
      case "regex" => PathMatcher.Regex(pattern.substring(colon + 1))
      case "glob" => glob(pattern.substring(colon + 1))
      case x => fail(pattern, s"'$x' is not supported")
    }
  }

  private def glob(glob: String): PathMatcher = {
    val res = new StringBuilder("^")

    val chars = glob.toCharArray
    var i = 0

    while (i < chars.length) {
      chars(i) match {
        case '*' => res.append(
            if (i + 1 >= chars.length || chars(i + 1) != '*') "[^/]*"
            else { i += 1; ".*" },
          )
        case '.' => res.append("\\.")
        case '?' => res.append('.')
        case '{' => res.append("(?:")
        case '}' => res.append(')')
        case ',' => res.append('|')
        case '/' => res.append('/')
        case '\\' => res.append("\\\\")
        case c => res.append(c)
      }
      i += 1
    }

    res.append("$") // Ensure full match
    PathMatcher.Regex(res.toString())
  }

}
