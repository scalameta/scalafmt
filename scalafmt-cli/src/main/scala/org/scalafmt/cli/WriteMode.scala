package org.scalafmt.cli

import scopt.Read

/**
  * Determines the mode in which Scalafmt will behave
  *
  * Override = Replace the file with its formatted form
  * Stdout = Print the formatted file to Stdout (leaving the original file untouched)
  */
sealed trait WriteMode

case object Override extends WriteMode
case object Stdout extends WriteMode
