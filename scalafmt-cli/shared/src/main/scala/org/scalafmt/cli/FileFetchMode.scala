package org.scalafmt.cli

import scopt.Read

/** Determines how we fetch files for formatting
  */
sealed trait FileFetchMode {
  def desc: String
}

object FileFetchMode {

  private val diffRefPrefix = "diff-ref="
  private val diffBase = "diff-base"
  private val diffBasePrefix = diffBase + "="
  private val availableModes = Seq(
    "diff" -> DiffFiles("master"),
    "changed" -> ChangedFiles,
    "any" -> RecursiveSearch,
    "anygit" -> GitFiles,
  )
  private val availableModesMap: Map[String, FileFetchMode] =
    availableModes.toMap

  def help: String =
    (Seq("diff-ref=xxx" -> DiffFiles("xxx"), diffBase -> DiffBase(None)) ++
      availableModes).map { case (k, v) => s"$k: ${v.desc}" }
      .mkString("   ", "\n   ", "")

  /** The read instance is practically is not exhaustive due to the
    * RecursiveSearch and GitFiles are the fallback used in the absence of other
    * options
    */
  implicit val read: Read[FileFetchMode] = Read.reads(x =>
    if (x.startsWith(diffRefPrefix))
      DiffFiles(x.substring(diffRefPrefix.length).trim)
    else if (x == diffBase) DiffBase(None)
    else if (x.startsWith(diffBasePrefix))
      DiffBase(Some(x.substring(diffBasePrefix.length).trim))
    else availableModesMap
      .getOrElse(x, throw new IllegalArgumentException(s"unknown mode: $x")),
  )

}

/** A simple recursive strategy where each directory is expanded
  */
case object RecursiveSearch extends FileFetchMode {
  def desc: String = "format any files found in current directory"
}

/** A call to `git ls-files --name-only <dir>`
  */
case object GitFiles extends FileFetchMode {
  def desc: String = "format any git-tracked files found in current directory"
}

/** A call to `git diff --name-only --diff-filter=d <branch>`
  *
  * When this is set, files passed via the cli are ignored.
  */
final case class DiffFiles(branch: String) extends FileFetchMode {
  def desc: String =
    s"format files listed in `git diff` against gitref `$branch`"
}

/** Files changed since HEAD forked from a base branch: `git diff` against
  * `git merge-base HEAD <ref>`, where `ref` defaults to the branch's configured
  * upstream (`@{upstream}`). If the scalafmt config changed since that fork
  * point, all tracked files are formatted instead (a config change can reformat
  * anything); likewise if no upstream is configured (nothing to diff against).
  *
  * When this is set, files passed via the cli are ignored.
  */
final case class DiffBase(ref: Option[String]) extends FileFetchMode {
  def desc: String = ref.fold(
    "format files changed since this branch forked from its upstream (@{upstream})",
  )(r => s"format files changed since this branch forked from `$r`")
}

/** A call to `git status --porcelain`
  *
  * When this is set, files passed via the cli are ignored.
  */
case object ChangedFiles extends FileFetchMode {
  def desc: String =
    "format files listed in `git status` (latest changes against previous commit)"
}
