package org.scalafmt.cli

import scopt.Read

/** Determines how we fetch files for formatting
  */
sealed trait FileFetchMode

object FileFetchMode {

  /** The read instance is practically is not exhaustive due to the
    * RecursiveSearch and GitFiles are the fallback used in the absence of other
    * options
    */
  implicit val read: Read[FileFetchMode] = Read.reads {
    case "diff" => DiffFiles("master")
    case "changed" => ChangedFiles
  }
}

/** A simple recursive strategy where each directory is expanded
  */
case object RecursiveSearch extends FileFetchMode

/** A call to `git ls-files --name-only <dir>`
  */
case object GitFiles extends FileFetchMode

/** A call to `git diff --name-only --diff-filter=d <branch>`
  *
  * When this is set, files passed via the cli are ignored.
  */
final case class DiffFiles(branch: String) extends FileFetchMode

/** A call to `git status --porcelain`
  *
  * When this is set, files passed via the cli are ignored.
  */
case object ChangedFiles extends FileFetchMode
