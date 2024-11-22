package org.scalafmt.internal

sealed abstract class Modification {
  val newlines: Int
  val length: Int
  @inline
  final def isNL: Boolean = newlines != 0
  @inline
  final def isBlankLine: Boolean = newlines > 1
}

case class Provided(ft: FT) extends Modification {
  override val newlines: Int = ft.newlinesBetween
  override lazy val length: Int =
    if (isNL) betweenText.indexOf('\n') else betweenText.length
  lazy val betweenText: String = ft.between.map(_.text).mkString
}

case object NoSplit extends Modification {
  override val newlines: Int = 0
  override val length: Int = 0
  def orNL(flag: Boolean): Modification = if (flag) this else Newline
  override def toString: String = "nS"
}

/** A split representing a newline.
  *
  * @param isDouble
  *   Insert a blank line?
  * @param noIndent
  *   Should no indentation follow? For example in commented out code.
  * @param alt
  *   Is it ok to replace this newline with a [[NoSplit]] or [[Space]] (with an
  *   optional additional set of indents) if the newline will indent beyond the
  *   current column? For example, used by select chains in [[Router]].
  */
case class NewlineT(isDouble: Boolean = false, noIndent: Boolean = false)
    extends Modification {
  override def toString = {
    val double = if (isDouble) "x2" else ""
    val indent = if (noIndent) "[NoIndent]" else ""
    "NL" + double + indent
  }
  override val newlines: Int = if (isDouble) 2 else 1
  override val length: Int = 0
}

object Newline extends NewlineT

object Newline2x extends NewlineT(isDouble = true) {
  def apply(isDouble: Boolean): NewlineT = if (isDouble) this else Newline
  def apply(ft: FT): NewlineT = apply(ft.hasBlankLine)
}

object NoIndentNewline extends NewlineT(noIndent = true)

object Newline2xNoIndent extends NewlineT(isDouble = true, noIndent = true)

object Space extends Modification {
  override val newlines: Int = 0
  override val length: Int = 1
  override def toString = "SP"

  def apply(flag: Boolean): Modification = if (flag) this else NoSplit
  def orNL(flag: Boolean): Modification = if (flag) this else Newline
  def orNL(nl: Int): Modification =
    if (FT.noBreak(nl)) this else Newline2x(FT.hasBlankLine(nl))
  def orNL(ft: FT): Modification = orNL(ft.newlinesBetween)
}

case class SpaceOrNoSplit(policy: Policy.End.WithPos) extends Modification {
  override val newlines: Int = 0
  override val length: Int = 1

  override def toString: String = "SPorNS"
}
