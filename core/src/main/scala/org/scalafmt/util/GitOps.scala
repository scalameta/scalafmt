package org.scalafmt.util

import scala.collection.breakOut
import scala.util.Try

import java.io.File

import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig

object FileExists {
  def unapply(arg: String): Option[File] = {
    val f = new File(arg)
    if (f.isFile) Some(f)
    else None
  }
}

object GitOps {
  import sys.process._
  def lsTree: Seq[String] =
    Try {
      Seq("git", "ls-tree", "-r", "HEAD", "--name-only").!!
        .split("\n")
        .toSeq
    }.getOrElse(Nil)

  def rootDir: Option[String] =
    Try {
      Seq("git", "rev-parse", "--show-toplevel").!!
    }.toOption

}
