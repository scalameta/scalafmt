package org.scalafmt.dynamic

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
  override def startLine: Int = pos.startLine
  override def startCharacter: Int = pos.startCharacter
  override def endLine: Int = pos.endLine
  override def endCharacter: Int = pos.endCharacter
}
