package org.scalafmt.util

import scala.sys.process.ProcessLogger
import scala.util.Try

import java.io.File

trait GitOps {
  def diff(branch: String): Seq[AbsoluteFile]
  def lsTree: Seq[AbsoluteFile]
  def rootDir: Option[AbsoluteFile]
}

object GitOps {
  def apply(): GitOps = new GitOpsImpl(AbsoluteFile.userDir)
}

class GitOpsImpl(workingDirectory: AbsoluteFile) extends GitOps {
  val swallowStderr = ProcessLogger(_ => Unit, _ => Unit)
  val baseCommand = Seq("git")

  def exec(cmd: Seq[String]): String = {
    sys.process
      .Process(cmd, workingDirectory.jfile)
      .!!(swallowStderr)
      .trim
  }

  override def lsTree: Seq[AbsoluteFile] =
    Try {
      exec(
        baseCommand ++ Seq(
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
      val cmd = baseCommand ++ Seq("rev-parse", "--show-toplevel")
      val result = AbsoluteFile.fromFile(new File(exec(cmd)), workingDirectory)
      require(result.jfile.isDirectory)
      result
    }.toOption

  override def diff(branch: String): Seq[AbsoluteFile] = {
    exec(baseCommand ++ Seq("diff", "--name-only", branch)).lines.map { x =>
      AbsoluteFile.fromFile(new File(x), workingDirectory)
    }.toSeq
  }
}
