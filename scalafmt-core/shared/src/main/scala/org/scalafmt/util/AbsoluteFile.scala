package org.scalafmt.util

import java.io.File
import java.nio.file.Path

import scala.io.Codec

/** Wrapper around java.io.File with an absolute path. */
sealed abstract case class AbsoluteFile(jfile: File) {
  def path: String = jfile.getPath
  def exists: Boolean = jfile.exists()
  def /(other: String) = join(FileOps.getFile(other))

  def join(other: Path) = new AbsoluteFile(asPath.resolve(other).toFile) {}
  def join(files: Seq[Path]): Seq[AbsoluteFile] = files.map(join)

  @inline
  def asPath: Path = jfile.toPath

  @inline
  def isRegularFile: Boolean = FileOps.isRegularFile(asPath)

  @inline def listFiles: Seq[AbsoluteFile] = FileOps.listFiles(jfile).map(/)
  @inline def readFile(implicit codec: Codec): String = FileOps.readFile(asPath)
  @inline def writeFile(content: String)(implicit codec: Codec): Unit =
    FileOps.writeFile(asPath, content)

  override def toString(): String = path
}

object AbsoluteFile {
  def fromPath(path: String): Option[AbsoluteFile] = {
    val file = new File(path)
    if (file.isAbsolute) Some(new AbsoluteFile(file) {})
    else None
  }
  def userDir = new AbsoluteFile(new File(System.getProperty("user.dir"))) {}
}
