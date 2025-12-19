package org.scalafmt.rewrite

import org.scalafmt.config._
import org.scalafmt.internal._

import scala.meta.tokens.Token.Constant

import java.lang

import scala.annotation.tailrec

object RewriteLiterals extends Rewrite with FormatTokensRewrite.RuleFactory {

  private def fpStyle(implicit style: ScalafmtConfig): Literals.FloatingPoint =
    style.literals.floatingPoint

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    fpStyle.filter.minTotalDigits < Int.MaxValue

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RewriteLiterals()

  private def prettyPrintFloatingPoint(
      value: BigDecimal,
      str: String,
      suffixLower: Char,
      suffixCase: Literals.FloatingPoint => Literals.Case,
  )(implicit style: ScalafmtConfig): Option[String] = {
    val fpFilter = fpStyle.filter

    val lastCh = str.last
    val signum = value.signum
    val hasSuffix = Character.toLowerCase(lastCh) == suffixLower
    val skip = fpFilter.needSuffix && !hasSuffix ||
      fpFilter.minTotalDigits >
      str.length - (if (hasSuffix) 1 else 0) - (if (signum < 0) 1 else 0)
    if (skip) return None

    val jsb = new lang.StringBuilder()
    def result() =
      if (jsb.length() == 0) None
      else {
        if (hasSuffix) jsb.append(suffixCase(fpStyle).process(lastCh))
        Some(jsb.toString)
      }

    if (signum == 0) {
      if (fpFilter.minSignificantDigits > 1) return None
      jsb.append('0')
      if (!hasSuffix) jsb.append(".0")
      return result()
    }

    val stripped = value.underlying().stripTrailingZeros()
    val digits = stripped.unscaledValue().abs().toString
    if (fpFilter.minSignificantDigits > digits.length) return None

    val fpFormat = fpStyle.format
    @tailrec
    def appendZeros(numZeros: Int): Int =
      if (numZeros > 0) {
        jsb.append('0')
        appendZeros(numZeros - 1)
      } else 0

    if (signum < 0) jsb.append('-')
    var exp = -stripped.scale()
    val beforeDot = digits.length - stripped.scale()
    if (exp >= 0 && exp <= fpFormat.maxPaddingZeros) {
      // all before dot, possibly padded
      jsb.append(digits)
      appendZeros(exp)
      if (!hasSuffix) jsb.append(".0")
    } else if (-beforeDot <= fpFormat.maxPaddingZeros && beforeDot <= 0) {
      // all after dot, possibly padded
      jsb.append("0.")
      appendZeros(-beforeDot)
      jsb.append(digits)
    } else if (beforeDot > 0 && beforeDot < digits.length)
      // split digits before and after dot
      jsb.append(digits, 0, beforeDot).append('.')
        .append(digits, beforeDot, digits.length)
    else {
      // use scientific
      exp = beforeDot - 1
      jsb.append(digits.charAt(0))
      if (digits.length > 1) {
        jsb.append('.')
        jsb.append(digits, 1, digits.length)
      }
      jsb.append(fpStyle.scientific match {
        case Literals.Case.Keep =>
          val exp = str.indexWhere(x => x == 'e' || x == 'E', digits.length)
          if (exp < 1) 'e' else str.charAt(exp)
        case lcase => lcase.process('e')
      })
      jsb.append(exp)
    }

    result()
  }

}

class RewriteLiterals(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import RewriteLiterals._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RewriteLiterals.enabled

  override def onToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = ft.right match {
    // if we replace, set end to be equal to start, that way we'll know it was rewritten
    case x @ Constant.Float(v) =>
      prettyPrintFloatingPoint(v, ft.meta.right.text, 'f', _.float)
        .map(replaceToken(_)(
          new Constant.Float(x.input, x.dialect, x.start, x.start, v),
        ))
    case x @ Constant.Double(v) =>
      prettyPrintFloatingPoint(v, ft.meta.right.text, 'd', _.double)
        .map(replaceToken(_)(
          new Constant.Double(x.input, x.dialect, x.start, x.start, v),
        ))
    case _ => None
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = None

}
