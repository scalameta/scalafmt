package org.scalafmt.util

import scala.collection.breakOut
import scala.sys.process.ProcessLogger
import scala.util.Try

import java.io.File

import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig

trait GitOps {
  def lsTree: Seq[String]
  def rootDir: Option[File]
}

object GitOps {
  def apply(): GitOps =
    new GitOpsImpl(new File(System.getProperty("user.dir")))
}

class GitOpsImpl(workingDirectory: File) extends GitOps {
  val swallowStderr = ProcessLogger(_ => Unit, _ => Unit)
  val baseCommand = Seq("git")

  def exec(cmd: Seq[String]): String = {
    sys.process
      .Process(cmd, workingDirectory)
      .!!(swallowStderr)
      .trim
  }

  override def lsTree: Seq[String] =
    Try {
      exec(
        baseCommand ++ Seq(
          "ls-tree",
          "-r",
          "HEAD",
          "--name-only"
        )
      ).split("\n").toSeq.collect {
        case f if new File(f).isFile =>
          new File(workingDirectory, f).getAbsolutePath
      }
    }.getOrElse(Nil)

  override def rootDir: Option[File] =
    Try {
      val cmd = baseCommand ++ Seq("rev-parse", "--show-toplevel")
      val result = new File(exec(cmd))
      require(result.isDirectory)
      result
    }.toOption

}
