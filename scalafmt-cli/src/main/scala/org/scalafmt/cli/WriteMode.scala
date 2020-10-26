package org.scalafmt.cli

/** Determines the mode in which Scalafmt will behave
  *
  * Override = Replace the file with its formatted form
  * Stdout = Print the formatted file to Stdout (leaving the original file untouched)
  */
sealed trait WriteMode

object WriteMode {

  case object Override extends WriteMode

  case object Stdout extends WriteMode

  case object List extends WriteMode

  case object Test extends WriteMode
}
