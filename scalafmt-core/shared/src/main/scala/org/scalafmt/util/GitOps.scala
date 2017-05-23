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
            dir.path
          )
        ).split(OsSpecific.lineSeparator).toSeq.collect {
          case f if new File(f).isFile => rtDir / f
        }
      }.getOrElse(Nil)
    }

  override def rootDir: Option[AbsoluteFile] =
    Try {
      val cmd = Seq(
        "git",
        "rev-parse",
        "--show-toplevel"
      )
      val result = AbsoluteFile.fromFile(new File(exec(cmd)), workingDirectory)
      require(result.jfile.isDirectory)
      result
    }.toOption

  override def diff(branch: String): Seq[AbsoluteFile] = {
    exec(
      Seq(
        "git",
        "diff",
        "--name-only",
        "--diff-filter=d",
        branch
      )).lines.map { x =>
      AbsoluteFile.fromFile(new File(x), workingDirectory)
    }.toSeq
  }
}
