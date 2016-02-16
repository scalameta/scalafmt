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

case object Newline extends NewlineT {}

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