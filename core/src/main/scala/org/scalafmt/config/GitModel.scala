package org.scalafmt.config

import java.io.File

import org.scalafmt.config
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitOps

case class GitModel(root: File, files: Seq[String]) {
  def getConfig: Option[metaconfig.Result[ScalafmtConfig]] = {
    val file = new File(root, ".scalafmt.conf")
    if (file.isFile)
      Option(config.Config.fromHocon(FileOps.readFile(file)))
    else None
  }
}

object GitModel {
  def get: Option[GitModel] = GitOps.rootDir.map { dir =>
    GitModel(new File(dir), GitOps.lsTree)
  }
}
