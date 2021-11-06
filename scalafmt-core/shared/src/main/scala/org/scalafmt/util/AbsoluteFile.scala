package org.scalafmt.util

import java.io.File
import java.nio.file.Path

/** Wrapper around java.io.File with an absolute path. */
sealed abstract case class AbsoluteFile(jfile: File) {
  def path: String = jfile.getAbsolutePath
  def exists: Boolean = jfile.exists()
  def /(other: String) = new AbsoluteFile(new File(jfile, other)) {}

  def join(other: Path) = new AbsoluteFile(asPath.resolve(other).toFile) {}
  def join(files: Seq[Path]): Seq[AbsoluteFile] = files.map(join)

  @inline
  def asPath: Path = jfile.toPath

  @inline
  def isRegularFile: Boolean = FileOps.isRegularFile(asPath)

  override def toString(): String = path
}

object AbsoluteFile {
  // If file is already absolute, then workingDir is not used.
  def fromFile(file: File, workingDir: AbsoluteFile): AbsoluteFile = {
    new AbsoluteFile(FileOps.makeAbsolute(workingDir.jfile)(file)) {}
  }
  def fromPath(path: String): Option[AbsoluteFile] = {
    val file = new File(path)
    if (file.isAbsolute) Some(new AbsoluteFile(file) {})
    else None
  }
  def userDir = new AbsoluteFile(new File(System.getProperty("user.dir"))) {}
}
