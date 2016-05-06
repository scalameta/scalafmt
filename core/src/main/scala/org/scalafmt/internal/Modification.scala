package org.scalafmt.internal

sealed abstract class Modification {

  def isNewline: Boolean = this match {
    case _: NewlineT => true
    case Provided(code) if code.contains('\n') => true
    case _ => false
  }

  def newlines: Int = this match {
    case n: NewlineT => if (n.isDouble) 2 else 1
    case Provided(code) => code.count(_ == '\n')
    case _ => 0
  }
}

case class Provided(code: String) extends Modification

case object NoSplit extends Modification

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
case class NewlineT(isDouble: Boolean = false,
                    noIndent: Boolean = false,
                    acceptNoSplit: Boolean = false)
    extends Modification {
  override def toString = {
    val double = if (isDouble) "Double" else ""
    val indent = if (noIndent) "NoIndent" else ""
    double + indent + "Newline"
  }
}

object Newline extends NewlineT {
  def apply: NewlineT = NewlineT()

  // TODO(olafur) remove method with NewlineT
  def apply(isDouble: Boolean, noIndent: Boolean = false): NewlineT =
    NewlineT(isDouble = isDouble, noIndent = noIndent)
}

object Newline2x extends NewlineT(isDouble = true)

object NoIndentNewline extends NewlineT(noIndent = true)

object Newline2xNoIndent extends NewlineT(isDouble = true, noIndent = true)

object Space extends Modification {
  override def toString = "Space"
}
