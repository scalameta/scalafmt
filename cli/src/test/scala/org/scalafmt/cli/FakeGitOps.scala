package org.scalafmt.cli

import java.io.File

import org.scalafmt.util.FileOps
import org.scalafmt.util.GitOps
import org.scalafmt.util.logger

class FakeGitOps(root: File) extends GitOps {
  override def lsTree = FileOps.listFiles(root)
  override def rootDir = Some(root)
}
