package org.scalafmt.sysops

import java.net.{URI, URL}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{AccessDeniedException, NoSuchFileException}
import java.nio.file.{Files, LinkOption, Path, Paths}
import scala.io.Codec
import scala.util.Using
import scala.util.{Failure, Success, Try}

import org.scalafmt.CompatCollections.JavaConverters._

object FileOps {

  val defaultConfigFileName = ".scalafmt.conf"

  def getLastModifiedMsec(file: Path): Long = {
    val attributes = getAttributesNoLinks(file)
    val mtime = attributes.lastModifiedTime().toMillis
    if (attributes.isSymbolicLink)
      math.max(mtime, Files.getLastModifiedTime(file).toMillis)
    else mtime
  }

  @inline
  def getLastModifiedMsecNoLinks(file: Path): Long =
    Files.getLastModifiedTime(file, LinkOption.NOFOLLOW_LINKS).toMillis

  @inline
  def isDirectory(file: Path): Boolean =
    Files.isDirectory(file)

  @inline
  def isDirectoryNoLinks(file: Path): Boolean =
    Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS)

  @inline
  def isRegularFile(file: Path): Boolean =
    Files.isRegularFile(file)

  @inline
  def isRegularFileNoLinks(file: Path): Boolean =
    Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)

  @inline
  def getAttributes(file: Path): BasicFileAttributes =
    Files.readAttributes(file, classOf[BasicFileAttributes])

  @inline
  def getAttributesNoLinks(file: Path): BasicFileAttributes =
    Files.readAttributes(
      file,
      classOf[BasicFileAttributes],
      LinkOption.NOFOLLOW_LINKS
    )

  def listFiles(path: String): Seq[Path] =
    listFiles(getFile(path))

  def listFiles(file: Path): Seq[Path] =
    listFiles(file, (_, a) => a.isRegularFile)

  def listFiles(
      file: Path,
      matches: (Path, BasicFileAttributes) => Boolean
  ): Seq[Path] = {
    val iter = Files.find(file, Integer.MAX_VALUE, (p, a) => matches(p, a))
    try iter.iterator().asScala.toList
    finally iter.close()
  }

  /** Reads file from file system or from http url */
  def readFile(filename: String)(implicit codec: Codec): String = {
    Try(new URL(filename)) match {
      case Success(url) => readFile(url)
      case _ => readFile(getFile(filename))
    }
  }

  def readFile(url: URL)(implicit codec: Codec): String = {
    val isFile = Option(url.getProtocol).forall("file".equalsIgnoreCase)
    if (isFile) readAsURI(url.toURI) else readAsURL(url)
  }

  @inline
  private[sysops] def readAsURL(url: URL)(implicit codec: Codec): String =
    Using.resource(scala.io.Source.fromURL(url)) {
      _.getLines().mkString("", "\n", "\n")
    }

  @inline
  private[sysops] def readAsURI(uri: URI)(implicit codec: Codec): String =
    readFile(Paths.get(uri))

  def readFile(file: Path)(implicit codec: Codec): String = {
    new String(Files.readAllBytes(file), codec.charSet)
  }

  @inline def getFile(path: String): Path = getPath(path)

  @inline def getFile(path: Seq[String]): Path =
    getPath(path.head, path.tail: _*)

  @inline def getPath(head: String, tail: String*): Path =
    Paths.get(head, tail: _*)

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = {
    val bytes = content.getBytes(codec.charSet)
    Files.write(path, bytes)
  }

  def writeFile(filename: String, content: String)(implicit
      codec: Codec
  ): Unit = {
    writeFile(getFile(filename), content)
  }

  @inline
  def isMarkdown(filename: String): Boolean = filename.endsWith(".md")

  @inline
  def isAmmonite(filename: String): Boolean = filename.endsWith(".sc")

  @inline
  def isSbt(filename: String): Boolean = filename.endsWith(".sbt")

  def getCanonicalConfigFile(
      workingDirectory: AbsoluteFile,
      config: Option[Path] = None
  ): Option[Try[Path]] =
    config.fold(tryGetConfigInDir(workingDirectory)) { x =>
      val file = workingDirectory.join(x)
      tryCheckConfigFile(file).orElse(
        Some(Failure(new NoSuchFileException(s"Config missing: $file")))
      )
    }

  def tryGetConfigInDir(dir: AbsoluteFile): Option[Try[Path]] =
    tryCheckConfigFile(dir / defaultConfigFileName)

  private def tryCheckConfigFile(file: AbsoluteFile): Option[Try[Path]] =
    if (!file.exists) None
    else if (file.isRegularFile) Some(Success(file.path))
    else Some(Failure(new AccessDeniedException(s"Config not a file: $file")))

  def getFileMatcher(paths: Seq[Path]): Path => Boolean = {
    val dirBuilder = Seq.newBuilder[Path]
    val fileBuilder = Set.newBuilder[Path]
    paths.foreach { path =>
      if (isRegularFile(path)) fileBuilder += path else dirBuilder += path
    }
    val dirs = dirBuilder.result()
    val files = fileBuilder.result()
    x =>
      files(x) || {
        val filename = x.toString
        val sep = x.getFileSystem.getSeparator
        dirs.exists { dir =>
          val dirname = dir.toString
          filename.startsWith(dirname) && {
            filename.length == dirname.length ||
            filename.startsWith(sep, dirname.length)
          }
        }
      }
  }

}
