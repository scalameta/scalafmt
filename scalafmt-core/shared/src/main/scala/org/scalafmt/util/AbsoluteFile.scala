package org.scalafmt.util

import java.io.File

/** Wrapper around java.io.File with an absolute path. */
sealed abstract case class AbsoluteFile(jfile: File) {
  def path: String = jfile.getAbsolutePath
  def /(other: String) = new AbsoluteFile(new File(jfile, other)) {}

  override def toString(): String = path
}

object AbsoluteFile {
  def fromFiles(files: Seq[File], workingDir: AbsoluteFile): Seq[AbsoluteFile] = {
    files.map(x => fromFile(x, workingDir))
  }
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
  def homeDir = new AbsoluteFile(new File(System.getProperty("user.home"))) {}
}
