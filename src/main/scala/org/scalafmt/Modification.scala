package org.scalafmt

sealed trait Modification {
  def isNewline = this match {
    case _: NewlineT => true
    case _ => false
  }
}

case object NoSplit extends Modification

trait NewlineT extends Modification {
  def isDouble: Boolean = false
  def noIndent: Boolean = false
}

object NewlineT {
  def apply(isDouble: Boolean, noIndent: Boolean): NewlineT =
    (isDouble, noIndent) match {
      case (true, true) => NoIndent2xNewline
      case (true, false) => Newline2x
      case (false, true) => NoIndentNewline
      case _ => Newline
    }
}


case object Newline extends NewlineT {
}

case object Newline2x extends NewlineT {
  override def isDouble: Boolean = true
}

case object NoIndentNewline extends NewlineT {
  override def noIndent: Boolean = true
}

case object NoIndent2xNewline extends NewlineT {
  override def noIndent: Boolean = true
  override def isDouble: Boolean = true
}

case object Space extends Modification


