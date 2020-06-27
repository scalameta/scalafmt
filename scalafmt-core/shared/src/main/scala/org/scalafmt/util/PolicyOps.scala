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

  /**
    * Forces allssplits up to including expire to be on a single line.
    */
  class SingleLineBlock private[PolicyOps] (
      f: Policy.Pf,
      endPos: Int,
      exclude: Set[Range]
  )(implicit line: sourcecode.Line)
      extends Policy.Clause(f, endPos, true) {
    override def toString: String =
      super.toString + {
        if (exclude.isEmpty) ""
        else exclude.map(x => s"${x.start}:${x.end}").mkString("^{", ",", "}")
      }
  }

  def SingleLineBlock(
      expire: T,
      exclude: Set[Range] = Set.empty,
      disallowSingleLineComments: Boolean = true,
      penaliseNewlinesInsideTokens: Boolean = false
  )(implicit line: sourcecode.Line): Policy = {
    import TokenOps.isSingleLineComment
    val endPos = expire.end
    val f: Policy.Pf = {
      case Decision(tok, s)
          if !tok.right.is[T.EOF] && tok.right.end <= endPos &&
            exclude.forall(!_.contains(tok.left.start)) &&
            (disallowSingleLineComments || !isSingleLineComment(tok.left)) =>
        if (penaliseNewlinesInsideTokens && tok.leftHasNewline)
          Seq.empty
        else
          s.filterNot(_.isNL)
    }
    new SingleLineBlock(f, endPos, exclude)
  }

}
