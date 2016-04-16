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

case class NewlineT(isDouble: Boolean = false, noIndent: Boolean = false)
    extends Modification

object Newline extends NewlineT {
  def apply: NewlineT = NewlineT()

  def apply(gets2x: Boolean, hasIndent: Boolean = false): NewlineT =
    (gets2x, hasIndent) match {
      case (true, true) => Newline2xNoIndent
      case (true, false) => Newline2x
      case (false, true) => NoIndentNewline
      case _ => Newline
    }
}

object Newline2x extends NewlineT(isDouble = true)

object NoIndentNewline extends NewlineT(noIndent = true)

object Newline2xNoIndent
    extends NewlineT(isDouble = true, noIndent = true)

object Space extends Modification
