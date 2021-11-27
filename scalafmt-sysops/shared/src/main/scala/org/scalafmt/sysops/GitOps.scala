package org.scalafmt.sysops

import scala.sys.process.ProcessLogger
import scala.util.{Failure, Success, Try}
import java.nio.file.Path

object GitOps {

  trait Factory {
    def apply(workingDirectory: AbsoluteFile): GitOps
  }

  object FactoryImpl extends Factory {
    def apply(workingDirectory: AbsoluteFile): GitOps =
      new GitOpsImpl(workingDirectory)
  }

  private def getMatchingFiles(
      files: Seq[AbsoluteFile],
      respectProjectFilters: Boolean,
      matches: AbsoluteFile => Boolean
  )(listDir: AbsoluteFile => Seq[AbsoluteFile]): Seq[AbsoluteFile] =
    files.flatMap {
      case d if d.isDirectory => listDir(d).filter(matches)
      // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
      // formatted regardless of what it is or where it is.
      // NB: Unless respectProjectFilters is also specified.
      case f if respectProjectFilters && !matches(f) => Seq.empty
      case f => Seq(f)
    }

  implicit class Implicit(obj: GitOps) {

    def getCanonicalConfigFile(config: Option[Path] = None): Option[Try[Path]] =
      FileOps
        .getCanonicalConfigFile(obj.workingDirectory, config)
        .orElse(obj.rootDir.flatMap(FileOps.tryGetConfigInDir))

    def getFiles(matches: AbsoluteFile => Boolean): Seq[AbsoluteFile] =
      obj.lsTree(obj.workingDirectory).filter(matches)

    def getFiles(
        files: Seq[AbsoluteFile],
        respectProjectFilters: Boolean,
        matches: AbsoluteFile => Boolean
    ): Seq[AbsoluteFile] =
      getMatchingFiles(files, respectProjectFilters, matches)(obj.lsTree)

    def getDirFiles(matches: AbsoluteFile => Boolean): Seq[AbsoluteFile] =
      obj.workingDirectory.listFiles.filter(matches)

    def getDirFiles(
        files: Seq[AbsoluteFile],
        respectProjectFilters: Boolean,
        matches: AbsoluteFile => Boolean
    ): Seq[AbsoluteFile] =
      getMatchingFiles(files, respectProjectFilters, matches)(_.listFiles)

    def getDiffFiles(
        branch: String,
        matches: AbsoluteFile => Boolean
    ): Seq[AbsoluteFile] =
      obj.diff(branch, None).filter(matches)

    def getDiffFiles(
        branch: String,
        respectProjectFilters: Boolean,
        matches: AbsoluteFile => Boolean
    )(files: Seq[AbsoluteFile]): Seq[AbsoluteFile] =
      getMatchingFiles(files, respectProjectFilters, matches)(x =>
        obj.diff(branch, Some(x))
      )

    def getDiffFiles(
        branch: String,
        files: Seq[Path],
        respectProjectFilters: Boolean,
        matches: AbsoluteFile => Boolean
    ): Seq[AbsoluteFile] =
      getDiffFiles(branch, respectProjectFilters, matches)(
        obj.workingDirectory.join(files)
      )

    def getChangedFiles(matches: AbsoluteFile => Boolean): Seq[AbsoluteFile] =
      obj.status.filter(matches)

  }

}

trait GitOps {
  val workingDirectory: AbsoluteFile
  def status: Seq[AbsoluteFile]
  def diff(branch: String, cwd: Option[AbsoluteFile]): Seq[AbsoluteFile]
  def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile]
  def rootDir: Option[AbsoluteFile]
}

private class GitOpsImpl(val workingDirectory: AbsoluteFile) extends GitOps {

  private[scalafmt] def exec(cmd: Seq[String]): Try[Seq[String]] = {
    val errors = Seq.newBuilder[String]
    Try {
      val swallowStderr = ProcessLogger(_ => (), errors += _)
      sys.process.Process(cmd, workingDirectory.jfile).!!(swallowStderr)
    }.fold(
      e => {
        val err = errors.result().mkString("\n> ", "\n> ", "\n")
        val msg = s"Failed to run command ${cmd.mkString(" ")}. Error:$err"
        Failure(new IllegalStateException(msg, e))
      },
      x => Success(x.trim.linesIterator.toSeq)
    )
  }

  override def lsTree(dir: AbsoluteFile): Seq[AbsoluteFile] = {
    val cmd = Seq("git", "ls-files", "--full-name", dir.toString())
    rootDir.fold(Seq.empty[AbsoluteFile]) { rtDir =>
      exec(cmd)
        .map(_.map(rtDir.join).filter(_.isRegularFile))
        .getOrElse(Seq.empty)
    }
  }

  override def rootDir: Option[AbsoluteFile] = {
    val cmd = Seq("git", "rev-parse", "--show-toplevel")
    for {
      Seq(rootPath) <- exec(cmd).toOption
      file <- AbsoluteFile.fromPathIfAbsolute(rootPath)
      if file.isDirectory
    } yield file
  }

  override def diff(
      branch: String,
      cwd: Option[AbsoluteFile]
  ): Seq[AbsoluteFile] = {
    val cmd = Seq("git", "diff", "--name-only", "--diff-filter=d", branch) ++
      cwd.map(x => Seq("--", x.toString())).getOrElse(Seq.empty)
    for {
      root <- rootDir.toSeq
      path <- exec(cmd).getOrElse(Seq.empty)
    } yield root.join(path)
  }

  override def status: Seq[AbsoluteFile] = {
    val cmd = Seq("git", "status", "--porcelain")
    for {
      root <- rootDir.toSeq
      statusLine <- exec(cmd).getOrElse(Seq.empty)
      file <- getFileFromGitStatusLine(statusLine).toOption.toSeq
    } yield root.join(file)
  }.filter(_.exists)

  private final val renameStatusCode = "R"
  private final val renameStatusArrowDelimiter = "-> "

  /*
    Method extracts path to changed file from the singular line of the `git status --porcelain` output.
   (see https://git-scm.com/docs/git-status#_short_format)
   */
  private def extractPathPart(s: String): Try[String] =
    Try(
      Option(s)
        // Checks if the line status states the file was renamed (E.g: `R  ORIG_PATH -> PATH`)
        .filter(_.substring(0, 2).contains(renameStatusCode))
        // takes the part of the string after the `-> ` character sequence
        .map(_.split(renameStatusArrowDelimiter).last)
        // fallback for the regular status line (E.g.: `XY PATH`)
        // Drops the status codes by splitting on white spaces then taking the tail of the result
        // Restores spaces in the path by merging the tail back with white space separator
        .getOrElse(s.trim.split(' ').tail.mkString(" "))
        .trim
    )

  private def trimQuotes(s: String): String =
    s.replaceAll("^\"|\"$", "")

  private def getFileFromGitStatusLine(s: String): Try[Path] =
    extractPathPart(s)
      .map(pathRaw => FileOps.getFile(trimQuotes(pathRaw)))
}
