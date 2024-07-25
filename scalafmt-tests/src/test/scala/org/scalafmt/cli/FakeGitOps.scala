package org.scalafmt.cli

import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.GitOps

class FakeGitOps(root: AbsoluteFile) extends GitOps {
  override def lsTree(dir: AbsoluteFile*): Seq[AbsoluteFile] = dir
    .flatMap(_.listFiles)
  override def rootDir: Option[AbsoluteFile] = Some(root)
  override def status(dir: AbsoluteFile*): Seq[AbsoluteFile] = lsTree(root)
  override def diff(branch: String, dir: AbsoluteFile*): Seq[AbsoluteFile] =
    lsTree(dir: _*)
  override def getAutoCRLF: Option[String] = None
}
