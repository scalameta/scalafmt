package org.scalafmt.sysops

import java.io.File
import java.net.URI
import java.nio.file.Path

import scala.io.Codec

/** Wrapper around java.io.File with an absolute path. */
final class AbsoluteFile(val path: Path) extends AnyVal {
  def exists: Boolean = PlatformFileOps.exists(path)
  def /(other: Path) = join(other)
  def /(other: String) = join(other)

  def join(other: Path) = new AbsoluteFile(path.resolve(other))
  def join(other: String) = new AbsoluteFile(path.resolve(other))
  def join(files: Seq[Path]): Seq[AbsoluteFile] = files.map(join)

  @inline
  def toUri: URI = path.toUri
  @inline
  def jfile: File = path.toFile

  @inline
  def isDirectory: Boolean = PlatformFileOps.isDirectory(path)
  @inline
  def isRegularFile: Boolean = PlatformFileOps.isRegularFile(path)
  @inline
  def isRegularFileNoLinks: Boolean = PlatformFileOps.isRegularFileNoLinks(path)

  @inline
  def listFiles: Seq[AbsoluteFile] = join(FileOps.listFiles(path))
  @inline
  def readFile(implicit codec: Codec): String = PlatformFileOps.readFile(path)
  @inline
  def writeFile(content: String)(implicit codec: Codec): Unit = PlatformFileOps
    .writeFile(path, content)

  @inline
  def parent: AbsoluteFile = new AbsoluteFile(path.getParent)
  @inline
  def delete(): Unit = PlatformFileOps.delete(path)
  @inline
  def mkdir(): Unit = PlatformFileOps.mkdir(path)
  @inline
  def mkdirs(): Unit = PlatformFileOps.mkdirs(path)

  override def toString: String = path.toString
  @inline
  def getFileName: String = path.getFileName.toString
}

object AbsoluteFile {

  def apply(file: File): AbsoluteFile = apply(file.toPath)
  def apply(path: Path): AbsoluteFile = new AbsoluteFile(path.toAbsolutePath)

  @inline
  def apply(path: Seq[String]): AbsoluteFile = apply(FileOps.getFile(path))
  @inline
  def apply(head: String, tail: String*): AbsoluteFile =
    apply(FileOps.getPath(head, tail: _*))

  def fromPathIfAbsolute(path: String): Option[AbsoluteFile] = {
    val file = FileOps.getPath(path)
    if (file.isAbsolute) Some(new AbsoluteFile(file)) else None
  }

  def userDir = AbsoluteFile(PlatformFileOps.cwd())
}
