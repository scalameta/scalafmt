package org.scalafmt.sysops

import java.nio.file.Path

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

  final class DirFiles(val cwd: AbsoluteFile)(
      val matches: Path => Boolean,
      skipDir: Path => Boolean = _ => false,
  ) extends BatchPathFinder {
    // prune fully-excluded subtrees during the walk instead of visiting and
    // filtering every file under them
    private val visitor = new FileOps.WalkVisitor {
      override def onTree(dir: Path, attrs: FileStat): FileOps.WalkVisit =
        if (skipDir(dir)) FileOps.WalkVisit.Skip else FileOps.WalkVisit.Good
      override def onFile(file: Path, attrs: FileStat): FileOps.WalkVisit =
        if (attrs.isRegularFile && matches(file)) FileOps.WalkVisit.Good
        else FileOps.WalkVisit.Skip
    }
    override def findFiles(dir: AbsoluteFile*): Seq[AbsoluteFile] = {
      val dirs = if (dir.isEmpty) Seq(cwd.path) else dir.map(_.path)
      dirs.flatMap(FileOps.walkFiles(visitor)).map(new AbsoluteFile(_))
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
