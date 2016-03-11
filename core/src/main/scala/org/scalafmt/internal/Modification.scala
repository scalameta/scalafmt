package org.scalafmt.internal

sealed abstract class Modification {

  def isNewline =
    this match {
      case _: NewlineT => true
      case _ => false
    }
}

case class Provided(code: String) extends Modification

case object NoSplit extends Modification

trait NewlineT extends Modification {

  def isDouble: Boolean = false

  def noIndent: Boolean = false
}

case object Newline extends NewlineT {

  def apply(gets2x: Boolean, hasIndent: Boolean = false): NewlineT =
    (gets2x, hasIndent) match {
      case (true, true) => Newline2xNoIndent
      case (true, false) => Newline2x
      case (false, true) => NoIndentNewline
      case _ => Newline
    }
}

case object Newline2x extends NewlineT {

  override def isDouble: Boolean = true
}

case object NoIndentNewline extends NewlineT {

  override def noIndent: Boolean = true
}

case object Newline2xNoIndent extends NewlineT {

  override def noIndent: Boolean = true

  override def isDouble: Boolean = true
}

case object Space extends Modification
