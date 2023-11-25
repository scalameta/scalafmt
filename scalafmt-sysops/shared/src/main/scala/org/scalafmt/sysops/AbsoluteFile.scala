package org.scalafmt.sysops

import java.io.File
import java.net.URI
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}

import scala.io.Codec

/** Wrapper around java.io.File with an absolute path. */
final class AbsoluteFile(val path: Path) extends AnyVal {
  def exists: Boolean = Files.exists(path)
  def /(other: Path) = join(other)
  def /(other: String) = join(other)

  def join(other: Path) = new AbsoluteFile(path.resolve(other))
  def join(other: String) = new AbsoluteFile(path.resolve(other))
  def join(files: Seq[Path]): Seq[AbsoluteFile] = files.map(join)

  @inline def toUri: URI = path.toUri
  @inline def jfile: File = path.toFile

  @inline def isDirectory: Boolean = FileOps.isDirectory(path)
  @inline def isRegularFile: Boolean = FileOps.isRegularFile(path)
  @inline def isRegularFileNoLinks: Boolean = FileOps.isRegularFileNoLinks(path)
  @inline def attributes: BasicFileAttributes = FileOps.getAttributes(path)

  @inline def listFiles: Seq[AbsoluteFile] = join(FileOps.listFiles(path))
  @inline def readFile(implicit codec: Codec): String = FileOps.readFile(path)
  @inline def writeFile(content: String)(implicit codec: Codec): Unit =
    FileOps.writeFile(path, content)

  @inline def parent: AbsoluteFile = new AbsoluteFile(path.getParent)
  @inline def delete(): Unit = Files.delete(path)
  @inline def mkdir(): Unit = Files.createDirectory(path)
  @inline def mkdirs(): Unit = Files.createDirectories(path)

  override def toString: String = path.toString
  @inline def getFileName: String = path.getFileName.toString
}

object AbsoluteFile {

  def apply(file: File): AbsoluteFile = apply(file.toPath)
  def apply(path: Path): AbsoluteFile = new AbsoluteFile(path.toAbsolutePath)

  @inline def apply(path: Seq[String]): AbsoluteFile =
    apply(FileOps.getFile(path))
  @inline def apply(head: String, tail: String*): AbsoluteFile =
    apply(FileOps.getPath(head, tail: _*))

  def fromPathIfAbsolute(path: String): Option[AbsoluteFile] = {
    val file = FileOps.getFile(path)
    if (file.isAbsolute) Some(new AbsoluteFile(file)) else None
  }

  def userDir = AbsoluteFile(System.getProperty("user.dir"))
}
