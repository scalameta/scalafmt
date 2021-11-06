package org.scalafmt.util

import scala.sys.process.ProcessLogger
import scala.util.Try
import scala.util.control.NonFatal
import java.nio.file.Path

object GitOps {

  trait Factory {
    def apply(workingDirectory: AbsoluteFile): GitOps
  }

  object FactoryImpl extends Factory {
    def apply(workingDirectory: AbsoluteFile): GitOps =
      new GitOpsImpl(workingDirectory)
  }

}

trait GitOps {
  def status: Seq[AbsoluteFile]
  def diff(branch: String, cwd: Option[AbsoluteFile]): Seq[AbsoluteFile]
  def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile]
  def rootDir: Option[AbsoluteFile]
}

private class GitOpsImpl(val workingDirectory: AbsoluteFile) extends GitOps {

  private[util] def exec(cmd: Seq[String]): Try[Seq[String]] = {
    val gitRes: Try[String] = Try {
      val lastError = new StringBuilder
      val swallowStderr = ProcessLogger(_ => (), err => lastError.append(err))
      try {
        sys.process
          .Process(cmd, workingDirectory.jfile)
          .!!(swallowStderr)
          .trim
      } catch {
        case NonFatal(e) =>
          throw new IllegalStateException(
            s"Failed to run command ${cmd.mkString(" ")}. " +
              s"Error: ${lastError.toString()}",
            e
          )
      }
    }
    gitRes.map(_.linesIterator.toSeq)
  }

  override def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile] = {
    val cmd = Seq(
      "git",
      "ls-files",
      "--full-name",
      dir.path
    )
    rootDir.fold(Seq.empty[AbsoluteFile]) { rtDir =>
      exec(cmd)
        .getOrElse(Seq.empty)
        .map(f => rtDir / f)
        .filter(_.isRegularFile)
    }
  }

  override def rootDir: Option[AbsoluteFile] = {
    val cmd = Seq(
      "git",
      "rev-parse",
      "--show-toplevel"
    )
    for {
      Seq(rootPath) <- exec(cmd).toOption
      file <- AbsoluteFile.fromPath(rootPath)
      if file.jfile.isDirectory
    } yield file
  }

  override def diff(
      branch: String,
      cwd: Option[AbsoluteFile]
  ): Seq[AbsoluteFile] = {
    val cmd = Seq(
      "git",
      "diff",
      "--name-only",
      "--diff-filter=d",
      branch
    ) ++ cwd.map(x => Seq("--", x.path)).getOrElse(Seq.empty)
    for {
      root <- rootDir.toSeq
      path <- exec(cmd).getOrElse(Seq.empty)
    } yield root / path
  }

  override def status: Seq[AbsoluteFile] = {
    val cmd = Seq(
      "git",
      "status",
      "--porcelain"
    )
    for {
      root <- rootDir.toSeq
      statusLine <- exec(cmd).getOrElse(Seq.empty)
      file <- getFileFromGitStatusLine(statusLine).toOption.toSeq
    } yield root.join(file)
  }.filter(_.exists)

  private final val renameStatusCode = "R"
  private final val renameStatusArrowDelimiter = "-> "

  /*
    Method extracts path to changed file from the singular line of the `git status --porcelain` output.
   (see https://git-scm.com/docs/git-status#_short_format)
   */
  private def extractPathPart(s: String): Try[String] =
    Try(
      Option(s)
        // Checks if the line status states the file was renamed (E.g: `R  ORIG_PATH -> PATH`)
        .filter(_.substring(0, 2).contains(renameStatusCode))
        // takes the part of the string after the `-> ` character sequence
        .map(_.split(renameStatusArrowDelimiter).last)
        // fallback for the regular status line (E.g.: `XY PATH`)
        // Drops the status codes by splitting on white spaces then taking the tail of the result
        // Restores spaces in the path by merging the tail back with white space separator
        .getOrElse(s.trim.split(' ').tail.mkString(" "))
        .trim
    )

  private def trimQuotes(s: String): String =
    s.replaceAll("^\"|\"$", "")

  private def getFileFromGitStatusLine(s: String): Try[Path] =
    extractPathPart(s)
      .map(pathRaw => FileOps.getFile(trimQuotes(pathRaw)))
}
