package org.scalafmt.cli.difflib

sealed abstract class Delta[T](original: Chunk[T], revised: Chunk[T]) {

  sealed abstract class TYPE
  object TYPE {
    case object CHANGE extends TYPE
    case object DELETE extends TYPE
    case object INSERT extends TYPE
  }
  def getType: TYPE
  def getOriginal: Chunk[T] = original
  def getRevised: Chunk[T] = revised

  override def toString: String = s"Delta($getType, $getOriginal, $getRevised)"
}
class ChangeDelta[T](original: Chunk[T], revised: Chunk[T])
    extends Delta(original, revised) {
  override def getType: TYPE = TYPE.CHANGE
}
class InsertDelta[T](original: Chunk[T], revised: Chunk[T])
    extends Delta(original, revised) {
  override def getType: TYPE = TYPE.INSERT
}
class DeleteDelta[T](original: Chunk[T], revised: Chunk[T])
    extends Delta(original, revised) {
  override def getType: TYPE = TYPE.DELETE
}
