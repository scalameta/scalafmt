package org.scalafmt.util

import scala.sys.process.ProcessLogger
import scala.util.Try
import scala.util.control.NonFatal

import java.io.File

trait GitOps {
  def diff(branch: String): Seq[AbsoluteFile]
  def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile]
  def rootDir: Option[AbsoluteFile]
}

object GitOps {
  def apply(): GitOps = new GitOpsImpl(AbsoluteFile.userDir)
}

class GitOpsImpl(workingDirectory: AbsoluteFile) extends GitOps {

  def exec(cmd: Seq[String]): String = {
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
          e)
    }
  }

  override def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile] =
    rootDir.fold(Seq.empty[AbsoluteFile]) { rtDir =>
      Try {
        exec(
          Seq(
            "git",
            "ls-tree",
            "-r",
            "HEAD",
            "--name-only",
            "--full-name",
            dir.path
          )
          // DESNOTE(2017-06-02, pjrt): Since the git command above only returns
          // files, no need to check if they are files
        ).split(OsSpecific.lineSeparator).toSeq.map(f => rtDir / f)
      }.getOrElse(Nil)
    }

  override def rootDir: Option[AbsoluteFile] = {
    val cmd = Seq(
      "git",
      "rev-parse",
      "--show-toplevel"
    )
    for {
      rootPath <- Try(exec(cmd)).toOption
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
    // DESNOTE(2017-06-02, pjrt): Since the git command above only returns
    // files, no need to check if they are files
    for {
      root <- rootDir.toSeq
      path <- Try(exec(cmd)).toOption.toSeq.flatMap(_.lines)
    } yield AbsoluteFile.fromFile(new File(path), root)
  }
}
