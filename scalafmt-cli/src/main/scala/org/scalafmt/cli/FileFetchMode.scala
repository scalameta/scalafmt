package org.scalafmt.cli

/**
  * Determines how we fetch files for formatting
  */
sealed trait FileFetchMode

/**
  * A simple recursive strategy where each directory is expanded
  */
final case object RecursiveSearch extends FileFetchMode

/**
  * A call to `git ls-files --name-only <dir>`
  */
final case object GitFiles extends FileFetchMode

/**
  * A call to `git diff --name-only --diff-filter=d <branch>`
  *
  * When this is set, files passed via the cli are ignored.
  */
final case class DiffFiles(branch: String) extends FileFetchMode
