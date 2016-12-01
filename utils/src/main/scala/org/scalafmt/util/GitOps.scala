package org.scalafmt.util

import scala.sys.process.ProcessLogger
import scala.util.Try

import java.io.File

trait GitOps {
  def diff(baseBranch: String): String
  def lsTree: Seq[AbsoluteFile]
  def rootDir: Option[AbsoluteFile]
}

object GitOps {
  def apply(): GitOps = new GitOpsImpl(AbsoluteFile.userDir)
}

class GitOpsImpl(workingDirectory: AbsoluteFile) extends GitOps {
  val swallowStderr = ProcessLogger(_ => Unit, _ => Unit)

  def exec(cmd: Seq[String]): String = {
    sys.process.Process(cmd, workingDirectory.jfile).!!(swallowStderr).trim
  }

  override def lsTree: Seq[AbsoluteFile] =
    Try {
      exec(
        Seq(
          "git",
          "ls-tree",
          "-r",
          "HEAD",
          "--name-only"
        )
      ).split("\n").toSeq.collect {
        case f if new File(f).isFile => workingDirectory / f
      }
    }.getOrElse(Nil)

  override def rootDir: Option[AbsoluteFile] =
    Try {
      val topdir = new File(exec(Seq("git", "rev-parse", "--show-toplevel")))
      val result = AbsoluteFile.fromFile(topdir, workingDirectory)
      require(result.jfile.isDirectory)
      result
    }.toOption

  override def diff(baseBranch: String): String = {
    exec(
      Seq(
        "git",
        "diff",
        "-U0",
        baseBranch
      )
    )
  }
}
