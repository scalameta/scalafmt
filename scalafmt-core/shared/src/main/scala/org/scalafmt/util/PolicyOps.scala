package org.scalafmt.util

import scala.meta.tokens.{Token => T}

import org.scalafmt.internal.Decision
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Policy

object PolicyOps {

  class PenalizeAllNewlines private[PolicyOps] (f: Policy.Pf, endPos: Int)(
      implicit line: sourcecode.Line
  ) extends Policy.Clause(f, endPos)

  def penalizeAllNewlines(
      expire: T,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      ignore: FormatToken => Boolean = _ => false,
      penaliseNewlinesInsideTokens: Boolean = false
  )(implicit line: sourcecode.Line): PenalizeAllNewlines = {
    val endPos = expire.end
    val f: Policy.Pf = {
      case Decision(tok, s)
          if tok.right.end < endPos &&
            (penalizeLambdas || !tok.left.is[T.RightArrow]) && !ignore(tok) =>
        s.map {
          case split
              if split.isNL ||
                (penaliseNewlinesInsideTokens && tok.leftHasNewline) =>
            split.withPenalty(penalty)
          case x => x
        }
    }
    new PenalizeAllNewlines(f, endPos)
  }

}
