package org.scalafmt.sysops

import java.io.IOException
import java.nio.file._

object DeleteTree {
  def apply(path: Path): Unit = {
    Files.walkFileTree(path, new DeleteTree)
    ()
  }
}

class DeleteTree extends FileVisitor[Path] {
  override def preVisitDirectory(
      dir: Path,
      attrs: attribute.BasicFileAttributes,
  ): FileVisitResult = FileVisitResult.CONTINUE

  override def visitFile(
      file: Path,
      attrs: attribute.BasicFileAttributes,
  ): FileVisitResult = {
    util.Try(Files.delete(file))
    FileVisitResult.CONTINUE
  }

  override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
    throw exc

  override def postVisitDirectory(
      dir: Path,
      exc: IOException,
  ): FileVisitResult = {
    util.Try(Files.delete(dir))
    FileVisitResult.CONTINUE
  }
}
