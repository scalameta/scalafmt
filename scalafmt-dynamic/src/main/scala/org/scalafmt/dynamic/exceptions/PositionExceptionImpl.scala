package org.scalafmt.dynamic.exceptions

import java.nio.file.Path
import org.scalafmt.interfaces.PositionException

case class PositionExceptionImpl(
    file: Path,
    code: String,
    shortMessage: String,
    longMessage: String,
    pos: RangePosition,
    cause: Throwable
) extends PositionException(longMessage, cause) {
  def start: Int = pos.start
  def end: Int = pos.end
  def startLine: Int = pos.startLine
  def startCharacter: Int = pos.startCharacter
  def endLine: Int = pos.endLine
  def endCharacter: Int = pos.endCharacter
}
