package org.scalafmt.util

import org.scalafmt.config.ScalafmtConfig

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
  )(implicit style: ScalafmtConfig): String = {
    val sb = new StringBuilder()
    val body =
      if (str.startsWith("0x") || str.startsWith("0X")) {
        sb.append("0x")
        str.drop(2)
      } else {
        str
      }

    if (body.endsWith("L") || body.endsWith("l")) {
      sb.append(style.literals.hex.process(body.dropRight(1)))
      sb.append(style.literals.long.process(body.takeRight(1)))
    } else {
      sb.append(style.literals.hex.process(body))
    }

    sb.toString()
  }

  /**
    * Prints float literals with specified case
    * */
  def prettyPrintFloat(str: String)(implicit style: ScalafmtConfig): String =
    style.literals.float.process(str)

  /**
    * Prints integer literals with specified case
    * */
  def prettyPrintDouble(str: String)(implicit style: ScalafmtConfig): String =
    style.literals.double.process(str)
}
