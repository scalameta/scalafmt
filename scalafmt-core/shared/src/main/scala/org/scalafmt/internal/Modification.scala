package org.scalafmt.internal

sealed abstract class Modification {
  val newlines: Int
  val length: Int
  @inline final def isNewline: Boolean = newlines != 0
}

case class Provided(code: String) extends Modification {
  override lazy val newlines: Int = code.count(_ == '\n')
  override lazy val length: Int = {
    val firstLine = code.indexOf('\n')
    if (firstLine == -1) code.length
    else firstLine
  }
}

case object NoSplit extends Modification {
  override val newlines: Int = 0
  override val length: Int = 0
  def orNL(flag: Boolean): Modification = if (flag) this else Newline
}

/**
  * A split representing a newline.
  *
  * @param isDouble Insert a blank line?
  * @param noIndent Should no indentation follow? For example in commented out
  *                 code.
  * @param acceptNoSplit Is it ok to replace this newline with a [[NoSplit]]
  *                      if the newline will indent beyond the current column?
  *                      For example, used by select chains in [[Router]].
  */
case class NewlineT(
    isDouble: Boolean = false,
    noIndent: Boolean = false,
    acceptSpace: Boolean = false,
    acceptNoSplit: Boolean = false
) extends Modification {
  override def toString = {
    val double = if (isDouble) "Double" else ""
    val indent = if (noIndent) "NoIndent" else ""
    double + indent + "Newline"
  }
  override val newlines: Int = if (isDouble) 2 else 1
  override val length: Int = 0
}

object Newline extends NewlineT {
  def apply: NewlineT = NewlineT()
}

object Newline2x extends NewlineT(isDouble = true)

object NoIndentNewline extends NewlineT(noIndent = true)

object Newline2xNoIndent extends NewlineT(isDouble = true, noIndent = true)

object Space extends Modification {
  override val newlines: Int = 0
  override val length: Int = 1
  override def toString = "Space"

  def apply(flag: Boolean): Modification = if (flag) this else NoSplit
  def orNL(flag: Boolean): Modification = if (flag) this else Newline
}
