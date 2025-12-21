package org.scalafmt.cli

/** Determines the mode in which Scalafmt will behave
  *
  *   - Override: Replace the file with its formatted form
  *   - Stdout: Print the formatted file to Stdout (leaving the original file
  *     untouched)
  */
sealed trait WriteMode {
  val usesOut: Boolean
}

object WriteMode {

  case object Override extends WriteMode {
    val usesOut: Boolean = false
  }

  case object Stdout extends WriteMode {
    val usesOut: Boolean = true
  }

  case object List extends WriteMode {
    val usesOut: Boolean = true
  }

  case object Test extends WriteMode {
    val usesOut: Boolean = false
  }

  case object DiffConfig extends WriteMode {
    val usesOut: Boolean = true
  }
}
