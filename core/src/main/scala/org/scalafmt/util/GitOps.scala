package org.scalafmt.util

import scala.collection.breakOut
import scala.util.Try

import java.io.File

import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig

trait GitOps {
  def lsTree: Seq[String]
  def rootDir: Option[File]
}

class GitOpsImpl extends GitOps {
  import sys.process._

  override def lsTree: Seq[String] =
    Try {
      Seq("git", "ls-tree", "-r", "HEAD", "--name-only").!!.split("\n").toSeq
    }.getOrElse(Nil)

  override def rootDir: Option[File] =
    Try {
      val result = new File(Seq("git", "rev-parse", "--show-toplevel").!!.trim)
      require(result.isDirectory)
      result
    }.toOption

}
