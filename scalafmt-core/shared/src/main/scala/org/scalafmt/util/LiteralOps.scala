package org.scalafmt.util

import org.scalafmt.config.{Case, ScalafmtConfig}

object LiteralOps {

  /**
    * Prints integer literals with specified case
    *
    * Divides literal into three parts:
    *
    * 0xFF123L
    * 0x is a hex prefix
    * FF123 is a body
    * L is a long suffix
    *
    * literals.hexPrefix applies prefix, literals.hexDigits applies to body
    * and literals.long applies to suffix
    * */
  def prettyPrintInteger(
      str: String
  )(implicit style: ScalafmtConfig): String =
    if (str.endsWith("L") || str.endsWith("l")) {
      prettyPrintHex(str.dropRight(1)) +
        style.literals.long.process(str.takeRight(1))
    } else {
      prettyPrintHex(str)
    }

  def prettyPrintFloat(str: String)(implicit style: ScalafmtConfig): String =
    prettyPrintFloatingPoint(str, 'F', 'f', style.literals.float)

  def prettyPrintDouble(str: String)(implicit style: ScalafmtConfig): String =
    prettyPrintFloatingPoint(str, 'D', 'd', style.literals.double)

  /**
    * Prints floating point literals with specified case
    *
    * Divides literals into two parts:
    *
    * 1.0e-10f
    * 1.0e-10 is a body with scientific notation
    * f is a float/double suffix
    *
    * literals.scientific applies to body and literals.float/double applies to suffix
    * */
  private def prettyPrintFloatingPoint(
      str: String,
      suffixUpper: Char,
      suffixLower: Char,
      suffixCase: Case
  )(implicit style: ScalafmtConfig): String =
    if (str.last == suffixUpper || str.last == suffixLower) {
      style.literals.scientific.process(str.dropRight(1)) +
        suffixCase.process(str.takeRight(1))
    } else {
      style.literals.scientific.process(str)
    }

  private def prettyPrintHex(
      str: String
  )(implicit style: ScalafmtConfig): String =
    if (str.startsWith("0x") || str.startsWith("0X")) {
      style.literals.hexPrefix.process(str.take(2)) +
        style.literals.hexDigits.process(str.drop(2))
    } else {
      str // not a hex literal
    }
}
