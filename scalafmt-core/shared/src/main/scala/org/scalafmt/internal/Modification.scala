package org.scalafmt.internal

sealed abstract class Modification {
  val newlines: Int
  val length: Int
  @inline
  final def isNL: Boolean = newlines != 0
  @inline
  final def isBlankLine: Boolean = newlines > 1
}

case class Provided(ft: FormatToken) extends Modification {
  override val newlines: Int = ft.newlinesBetween
  override lazy val length: Int =
    if (isNL) betweenText.indexOf('\n') else betweenText.length
  lazy val betweenText: String = ft.between.map(_.syntax).mkString
}

case object NoSplit extends Modification {
  override val newlines: Int = 0
  override val length: Int = 0
  def orNL(flag: Boolean): Modification = if (flag) this else Newline
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
case class NewlineT(
    isDouble: Boolean = false,
    noIndent: Boolean = false,
    alt: Option[ModExt] = None,
) extends Modification {
  override def toString = {
    val double = if (isDouble) "Double" else ""
    val indent = if (noIndent) "NoIndent" else ""
    val altStr = alt.fold("")(x => "|" + x.mod.toString)
    double + indent + "Newline" + altStr
  }
  override val newlines: Int = if (isDouble) 2 else 1
  override val length: Int = 0
}

object Newline extends NewlineT

object Newline2x extends NewlineT(isDouble = true) {
  def apply(isDouble: Boolean): NewlineT = if (isDouble) this else Newline
  def apply(ft: FormatToken): NewlineT = apply(ft.hasBlankLine)
}

object NoIndentNewline extends NewlineT(noIndent = true)

object Newline2xNoIndent extends NewlineT(isDouble = true, noIndent = true)

object Space extends Modification {
  override val newlines: Int = 0
  override val length: Int = 1
  override def toString = "Space"

  def apply(flag: Boolean): Modification = if (flag) this else NoSplit
  def orNL(flag: Boolean): Modification = if (flag) this else Newline
  def orNL(nl: Int): Modification =
    if (FormatToken.noBreak(nl)) this
    else Newline2x(FormatToken.hasBlankLine(nl))
}
