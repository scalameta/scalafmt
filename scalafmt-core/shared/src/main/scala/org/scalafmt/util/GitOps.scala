package org.scalafmt.util

import scala.sys.process.ProcessLogger
import scala.util.Try
import scala.util.control.NonFatal

import java.io.File

trait GitOps {
  def status: Seq[AbsoluteFile]
  def diff(branch: String): Seq[AbsoluteFile]
  def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile]
  def rootDir: Option[AbsoluteFile]
}

object GitOps {
  def apply(): GitOps = new GitOpsImpl(AbsoluteFile.userDir)
}

class GitOpsImpl(private[util] val workingDirectory: AbsoluteFile)
    extends GitOps {

  private[util] def exec(cmd: Seq[String]): Try[Seq[String]] = {
    val gitRes: Try[String] = Try {
      val lastError = new StringBuilder
      val swallowStderr = ProcessLogger(_ => Unit, err => lastError.append(err))
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
    // Predef.augmentString = work around scala/bug#11125 on JDK 11
    gitRes.map(augmentString(_).lines.toSeq)
  }

  override def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile] =
    rootDir.fold(Seq.empty[AbsoluteFile]) { rtDir =>
      exec(
        Seq(
          "git",
          "ls-files",
          "--full-name",
          dir.path
        )
      ).getOrElse(Seq.empty).map(f => rtDir / f)
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

  override def diff(branch: String): Seq[AbsoluteFile] = {
    val cmd = Seq(
      "git",
      "diff",
      "--name-only",
      "--diff-filter=d",
      branch
    )
    for {
      root <- rootDir.toSeq
      path <- exec(cmd).getOrElse(Seq.empty)
    } yield AbsoluteFile.fromFile(new File(path), root)
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
      file <- getFileFromGitStatusLine(statusLine).toSeq
    } yield AbsoluteFile.fromFile(file, root)
  }.filter(_.exists)

  private final val statusCodesSubstringLength = 3
  private final val moveStatusArrowDelimiter = "-> "

  private def getFileFromGitStatusLine(s: String): Option[File] =
    s.substring(statusCodesSubstringLength)
      .split(moveStatusArrowDelimiter) match {
      case Array(path) => Try(new File(path)).toOption
      case Array(_, path) => Try(new File(path)).toOption
      case _ => None
    }
}
