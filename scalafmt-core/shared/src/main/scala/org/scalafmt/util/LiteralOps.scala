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
    * literals.long applies to suffix and literals.hex applies to body
    * */
  def prettyPrintInteger(
      str: String
  )(implicit style: ScalafmtConfig): String =
    if (str.endsWith("L") || str.endsWith("l")) {
      style.literals.hex.process(str.dropRight(1)) +
        style.literals.long.process(str.takeRight(1))
    } else {
      style.literals.hex.process(str)
    }

  def prettyPrintFloat(str: String)(implicit style: ScalafmtConfig): String =
    prettyPrintFloatingPoint(str, "F", "f", style.literals.float)

  def prettyPrintDouble(str: String)(implicit style: ScalafmtConfig): String =
    prettyPrintFloatingPoint(str, "D", "d", style.literals.double)

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
      suffixUpper: String,
      suffixLower: String,
      suffixCase: Case
  )(implicit style: ScalafmtConfig): String =
    if (str.endsWith(suffixUpper) || str.endsWith(suffixLower)) {
      style.literals.scientific.process(str.dropRight(1)) +
        suffixCase.process(str.takeRight(1))
    } else {
      style.literals.scientific.process(str)
    }
}
