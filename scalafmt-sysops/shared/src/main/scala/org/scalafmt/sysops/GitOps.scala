package org.scalafmt.sysops

import java.nio.file.Path

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object GitOps {

  trait Factory {
    def apply(workingDirectory: AbsoluteFile): GitOps
  }

  object FactoryImpl extends Factory {
    def apply(workingDirectory: AbsoluteFile): GitOps =
      new GitOpsImpl(workingDirectory)
  }

  implicit class Implicit(private val obj: GitOps) extends AnyVal {

    def getCanonicalConfigFile(
        cwd: AbsoluteFile,
        config: Option[Path] = None,
    ): Option[Try[Path]] = FileOps.getCanonicalConfigFile(cwd, config)
      .orElse(getRootConfigFile)

    def getRootConfigFile: Option[Try[Path]] = obj.rootDir
      .flatMap(FileOps.tryGetConfigInDir)

    def getProposedConfigFile(
        cwd: AbsoluteFile,
        config: Option[Path] = None,
    ): AbsoluteFile = config
      .fold(obj.rootDir.getOrElse(cwd) / FileOps.defaultConfigFileName)(cwd / _)

  }

  class GitException(message: String) extends Exception(message)
}

trait GitOps {
  def status(dir: AbsoluteFile*): Seq[AbsoluteFile]
  def diff(branch: String, dir: AbsoluteFile*): Seq[AbsoluteFile]
  def lsTree(dir: AbsoluteFile*): Seq[AbsoluteFile]
  def rootDir: Option[AbsoluteFile]
  def getAutoCRLF: Option[String]
}

private class GitOpsImpl(val workingDirectory: AbsoluteFile) extends GitOps {

  private[scalafmt] def tryExecLines(cmd: Seq[String]): Try[Seq[String]] =
    PlatformRunOps.runArgv(cmd, Some(workingDirectory.path))

  private[scalafmt] def exec(cmd: Seq[String]): Seq[String] = tryExecLines(cmd)
    .get

  override def lsTree(dir: AbsoluteFile*): Seq[AbsoluteFile] = {
    val cmd = Seq("git", "ls-files", "--full-name") ++ dir.map(_.toString())
    withRoot(exec(cmd)).filter(_.isRegularFileNoLinks)
  }

  override def rootDir: Option[AbsoluteFile] = tryRoot.toOption

  private lazy val tryRoot: Try[AbsoluteFile] = {
    val cmd = Seq("git", "rev-parse", "--show-toplevel")
    def failed(msg: String) = Failure(new GitOps.GitException(msg))
    tryExecLines(cmd).flatMap(_.headOption match {
      case Some(x) =>
        val file = AbsoluteFile(x)
        if (file.isDirectory) Success(file) else failed(s"not a directory: $x")
      case _ => failed("unable to determine git root")
    })
  }

  override def diff(branch: String, dir: AbsoluteFile*): Seq[AbsoluteFile] = {
    val cmd = Seq("git", "diff", "--name-only", "--diff-filter=d", branch) ++
      (if (dir.isEmpty) Seq.empty else "--" +: dir.map(_.toString()))
    withRoot(exec(cmd))
  }

  override def status(dir: AbsoluteFile*): Seq[AbsoluteFile] = {
    val cmd = Seq("git", "status", "--porcelain") ++
      (if (dir.isEmpty) Seq.empty else "--" +: dir.map(_.toString()))
    withRoot(exec(cmd).map(getFileFromGitStatusLine)).filter(_.exists)
  }

  override def getAutoCRLF: Option[String] = {
    val cmd = Seq("git", "config", "--get", "core.autocrlf")
    tryExecLines(cmd).toOption.flatMap(_.headOption)
  }

  private final val renameStatusCode = "R"
  private final val renameStatusArrowDelimiter = "-> "

  private def withRoot(files: => Seq[String]): Seq[AbsoluteFile] = {
    val root = tryRoot.get
    files.map(root.join)
  }

  /*
    Method extracts path to changed file from the singular line of the `git status --porcelain` output.
   (see https://git-scm.com/docs/git-status#_short_format)
   */
  private def extractPathPart(s: String): String = Option(s)
    // Checks if the line status states the file was renamed (E.g: `R  ORIG_PATH -> PATH`)
    .filter(_.substring(0, 2).contains(renameStatusCode))
    // takes the part of the string after the `-> ` character sequence
    .map(_.split(renameStatusArrowDelimiter).last)
    // fallback for the regular status line (E.g.: `XY PATH`)
    // Drops the status codes by splitting on white spaces then taking the tail of the result
    // Restores spaces in the path by merging the tail back with white space separator
    .getOrElse(s.trim.split(' ').tail.mkString(" ")).trim

  private def trimQuotes(s: String): String = s.replaceAll("^\"|\"$", "")

  private def getFileFromGitStatusLine(s: String): String =
    trimQuotes(extractPathPart(s))
}
