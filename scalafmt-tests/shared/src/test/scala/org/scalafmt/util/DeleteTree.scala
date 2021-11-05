package org.scalafmt.util

import java.io.IOException
import java.nio.file.{FileVisitResult, FileVisitor, Files, Path}
import java.nio.file.attribute.BasicFileAttributes

object DeleteTree {
  def deleteTree(path: Path): Unit = {
    Files.walkFileTree(path, new DeleteTree)
    ()
  }
}

class DeleteTree extends FileVisitor[Path] {
  override def preVisitDirectory(
      dir: Path,
      attrs: BasicFileAttributes
  ): FileVisitResult = {
    FileVisitResult.CONTINUE
  }

  override def visitFile(
      file: Path,
      attrs: BasicFileAttributes
  ): FileVisitResult = {
    Files.delete(file)
    FileVisitResult.CONTINUE
  }

  override def visitFileFailed(
      file: Path,
      exc: IOException
  ): FileVisitResult = {
    throw exc
  }

  override def postVisitDirectory(
      dir: Path,
      exc: IOException
  ): FileVisitResult = {
    Files.delete(dir)
    FileVisitResult.CONTINUE
  }
}
