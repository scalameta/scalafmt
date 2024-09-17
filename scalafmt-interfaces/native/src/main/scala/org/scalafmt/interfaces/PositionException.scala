package org.scalafmt.interfaces

import java.nio.file.Path

/** An exception that happened at a position in a source file such as a parse
  * error.
  */
abstract class PositionException(message: String, cause: Throwable)
    extends Exception(message, cause) {

  override def fillInStackTrace(): Throwable = synchronized(return this)

  /** @return
    *   The file where the error occurred.
    */
  def file(): Path

  /** @return
    *   The text contents of the file being formatted.
    */
  def code(): String

  /** @return
    *   The fully formatted error message including line content and caret.
    */
  def longMessage(): String

  /** @return
    *   Only the error message itself without line content and caret.
    */
  def shortMessage(): String

  def startLine(): Int
  def startCharacter(): Int
  def endLine(): Int
  def endCharacter(): Int
}
