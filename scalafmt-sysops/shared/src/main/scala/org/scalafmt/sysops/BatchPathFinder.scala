package org.scalafmt.sysops

import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes

trait BatchPathFinder {

  def matches: Path => Boolean
  def findFiles(dir: AbsoluteFile*): Seq[AbsoluteFile]

  final def findFilesExplicit(dirs: Seq[AbsoluteFile]): Seq[AbsoluteFile] =
    if (dirs.isEmpty) Seq.empty else findFiles(dirs: _*)

  final def findMatchingFiles(
      filterFilesToo: Boolean,
      paths: AbsoluteFile*,
  ): Seq[AbsoluteFile] =
    if (paths.isEmpty) findFiles()
    else {
      val files = Seq.newBuilder[AbsoluteFile]
      val dirs = Seq.newBuilder[AbsoluteFile]
      paths.foreach(x =>
        if (!x.isRegularFile) dirs += x
        // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
        // formatted regardless of what it is or where it is.
        // NB: Unless respectProjectFilters is also specified.
        else if (!filterFilesToo || matches(x.path)) files += x,
      )
      files.result() ++ findFilesExplicit(dirs.result())
    }
}

object BatchPathFinder {

  final class DirFiles(val cwd: AbsoluteFile)(val matches: Path => Boolean)
      extends BatchPathFinder {
    private def filter(path: Path, attrs: BasicFileAttributes): Boolean =
      attrs.isRegularFile && matches(path)
    override def findFiles(dir: AbsoluteFile*): Seq[AbsoluteFile] = {
      val dirs = if (dir.isEmpty) Seq(cwd.path) else dir.map(_.path)
      dirs.flatMap(FileOps.listFiles(_, filter)).map(new AbsoluteFile(_))
    }
  }

  final class GitFiles(git: GitOps)(val matches: Path => Boolean)
      extends BatchPathFinder {
    override def findFiles(dir: AbsoluteFile*): Seq[AbsoluteFile] = git
      .lsTree(dir: _*).filter(x => matches(x.path))
  }

  final class GitBranchFiles(git: GitOps, branch: String)(
      val matches: Path => Boolean,
  ) extends BatchPathFinder {
    override def findFiles(dir: AbsoluteFile*): Seq[AbsoluteFile] = git
      .diff(branch, dir: _*).filter(x => matches(x.path))
  }

  final class GitDirtyFiles(git: GitOps)(val matches: Path => Boolean)
      extends BatchPathFinder {
    override def findFiles(dir: AbsoluteFile*): Seq[AbsoluteFile] = git
      .status(dir: _*).filter(x => matches(x.path))
  }

}
