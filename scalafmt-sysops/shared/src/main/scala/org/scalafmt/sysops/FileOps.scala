package org.scalafmt.sysops

import org.scalafmt.CompatCollections.JavaConverters._

import java.nio.file.AccessDeniedException
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.NoSuchFileException
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.BasicFileAttributes

import scala.io.Codec
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object FileOps {

  val defaultConfigFileName = ".scalafmt.conf"

  @inline
  def isDirectory(file: Path): Boolean = Files.isDirectory(file)

  @inline
  def isRegularFile(file: Path): Boolean = Files.isRegularFile(file)

  @inline
  def isRegularFileNoLinks(file: Path): Boolean = Files
    .isRegularFile(file, LinkOption.NOFOLLOW_LINKS)

  @inline
  def getAttributes(file: Path): BasicFileAttributes = Files
    .readAttributes(file, classOf[BasicFileAttributes])

  def listFiles(path: String): Seq[Path] = listFiles(getFile(path))

  def listFiles(file: Path): Seq[Path] =
    listFiles(file, (_, a) => a.isRegularFile)

  def listFiles(
      file: Path,
      matches: (Path, BasicFileAttributes) => Boolean,
  ): Seq[Path] = {
    val iter = Files.find(file, Integer.MAX_VALUE, (p, a) => matches(p, a))
    try iter.iterator().asScala.toList
    finally iter.close()
  }

  def readFile(file: Path)(implicit codec: Codec): String =
    new String(Files.readAllBytes(file), codec.charSet)

  @inline
  def getFile(path: String): Path = getPath(path)

  @inline
  def getFile(path: Seq[String]): Path = getPath(path.head, path.tail: _*)

  @inline
  def getPath(head: String, tail: String*): Path = Paths.get(head, tail: _*)

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = {
    val bytes = content.getBytes(codec.charSet)
    Files.write(path, bytes)
  }

  def writeFile(filename: String, content: String)(implicit
      codec: Codec,
  ): Unit = writeFile(getFile(filename), content)

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
      .orElse(Some(Failure(new NoSuchFileException(s"Config missing: $file"))))
  }

  def tryGetConfigInDir(dir: AbsoluteFile): Option[Try[Path]] =
    tryCheckConfigFile(dir / defaultConfigFileName)

  private def tryCheckConfigFile(file: AbsoluteFile): Option[Try[Path]] =
    if (!file.exists) None
    else if (file.isRegularFile) Some(Success(file.path))
    else Some(Failure(new AccessDeniedException(s"Config not a file: $file")))

}
