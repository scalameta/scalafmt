package org.scalafmt.cli

import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitOps

class FakeGitOps(root: AbsoluteFile) extends GitOps {
  override def lsTree(dir: AbsoluteFile): Vector[AbsoluteFile] =
    FileOps.listFiles(dir)
  override def rootDir: Option[AbsoluteFile] = Some(root)
  override def diff(branch: String): Seq[AbsoluteFile] = lsTree(root)
}
