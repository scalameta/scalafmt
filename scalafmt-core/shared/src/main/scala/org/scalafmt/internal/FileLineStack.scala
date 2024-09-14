package org.scalafmt.internal

import org.scalameta.FileLine

class FileLineStack private (
    val fileLineHead: FileLine,
    val fileLineLast: FileLine,
) extends Ordered[FileLineStack] {
  override def toString: String = s"$fileLineHead->$fileLineLast"

  def forThisLine(implicit
      file: sourcecode.File,
      line: sourcecode.Line,
  ): FileLineStack = new FileLineStack(
    fileLineHead = fileLineHead,
    fileLineLast = FileLine.generate,
  )

  override def compare(that: FileLineStack): Int = Integer
    .compare(this.fileLineHead.line.value, that.fileLineHead.line.value)
}

object FileLineStack {
  implicit def generate(implicit
      file: sourcecode.File,
      line: sourcecode.Line,
      fileLineHead: FileLine,
  ): FileLineStack = new FileLineStack(fileLineHead, FileLine.generate)

  def nextLine(implicit fl: FileLine): FileLine = {
    val line = fl.line
    new FileLine(fl.file, line.copy(value = line.value + 1))
  }
}
