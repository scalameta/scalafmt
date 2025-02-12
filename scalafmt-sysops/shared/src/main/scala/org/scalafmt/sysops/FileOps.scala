package org.scalafmt.sysops

import java.nio.file.Path
import java.nio.file.Paths

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object FileOps {

  val defaultConfigFileName = ".scalafmt.conf"

  def listFiles(path: String): Seq[Path] = listFiles(getPath(path))

  def listFiles(file: Path): Seq[Path] =
    listFiles((_, a) => a.isRegularFile)(file)

  def listFiles(matches: (Path, FileStat) => Boolean)(file: Path): Seq[Path] =
    PlatformFileOps.listFiles(file, matches)

  @inline
  def getFile(path: Seq[String]): Path = getPath(path.head, path.tail: _*)

  @inline
  def getPath(head: String, tail: String*): Path = Paths.get(head, tail: _*)

  @inline
  def isMarkdown(filename: String): Boolean = filename.endsWith(".md")

  @inline
  def isAmmonite(filename: String): Boolean = filename.endsWith(".sc")

  @inline
  def isSbt(filename: String): Boolean = filename.endsWith(".sbt")

  def getCanonicalConfigFile(
      workingDirectory: AbsoluteFile,
      config: Option[Path] = None,
  ): Option[Try[Path]] = config.fold(tryGetConfigInDir(workingDirectory)) { x =>
    val file = workingDirectory.join(x)
    tryCheckConfigFile(file)
      .orElse(Some(Failure(new RuntimeException(s"Config missing: $file"))))
  }

  def tryGetConfigInDir(dir: AbsoluteFile): Option[Try[Path]] =
    tryCheckConfigFile(dir / defaultConfigFileName)

  private def tryCheckConfigFile(file: AbsoluteFile): Option[Try[Path]] =
    if (!file.exists) None
    else if (file.isRegularFile) Some(Success(file.path))
    else Some(Failure(new RuntimeException(s"Config not a file: $file")))

}
