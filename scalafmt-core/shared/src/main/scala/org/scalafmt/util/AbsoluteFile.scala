package org.scalafmt.util

import java.io.File
import java.net.URI
import java.nio.file.Path

import scala.io.Codec

/** Wrapper around java.io.File with an absolute path. */
final class AbsoluteFile(val jfile: File) extends AnyVal {
  def exists: Boolean = jfile.exists()
  def /(other: String) = join(FileOps.getFile(other))

  def join(other: Path) = new AbsoluteFile(path.resolve(other).toFile)
  def join(files: Seq[Path]): Seq[AbsoluteFile] = files.map(join)

  @inline def toUri: URI = jfile.toURI
  @inline def path: Path = jfile.toPath

  @inline def isRegularFile: Boolean = FileOps.isRegularFile(path)

  @inline def listFiles: Seq[AbsoluteFile] = join(FileOps.listFiles(jfile))
  @inline def readFile(implicit codec: Codec): String = FileOps.readFile(path)
  @inline def writeFile(content: String)(implicit codec: Codec): Unit =
    FileOps.writeFile(path, content)

  override def toString(): String = jfile.getPath
  @inline def getFileName: String = jfile.getName()
}

object AbsoluteFile {
  def fromPath(path: String): Option[AbsoluteFile] = {
    val file = new File(path)
    if (file.isAbsolute) Some(new AbsoluteFile(file))
    else None
  }
  def userDir = new AbsoluteFile(new File(System.getProperty("user.dir")))
}
