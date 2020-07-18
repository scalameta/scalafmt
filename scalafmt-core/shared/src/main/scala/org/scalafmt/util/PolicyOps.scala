package org.scalafmt.util

import scala.meta.tokens.{Token => T}

import org.scalafmt.internal.Decision
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Policy

object PolicyOps {

  case class PenalizeAllNewlines(
      expire: T,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      ignore: FormatToken => Boolean = _ => false,
      penaliseNewlinesInsideTokens: Boolean = false
  )(implicit line: sourcecode.Line)
      extends Policy.Clause {
    override val noDequeue: Boolean = false
    override val endPolicy: Policy.End.WithPos = Policy.End.Before(expire.end)
    override val f: Policy.Pf = {
      case Decision(tok, s)
          if (penalizeLambdas || !tok.left.is[T.RightArrow]) && !ignore(tok) =>
        s.map {
          case split
              if split.isNL ||
                (penaliseNewlinesInsideTokens && tok.leftHasNewline) =>
            split.withPenalty(penalty)
          case x => x
        }
    }
    override def toString: String = s"PNL:${super.toString}+$penalty"
  }

  /**
    * Forces allssplits up to including expire to be on a single line.
    */
  case class SingleLineBlock(
      expire: T,
      exclude: Set[Range] = Set.empty,
      disallowSingleLineComments: Boolean = true,
      penaliseNewlinesInsideTokens: Boolean = false
  )(implicit line: sourcecode.Line)
      extends Policy.Clause {
    import TokenOps.isSingleLineComment
    private val endPos = expire.end
    override val noDequeue: Boolean = true
    override val endPolicy: Policy.End.WithPos = Policy.End.On(endPos)
    override def toString: String =
      "SLB:" + super.toString + {
        if (exclude.isEmpty) ""
        else exclude.map(x => s"${x.start}:${x.end}").mkString("^{", ",", "}")
      }
    override val f: Policy.Pf = {
      case Decision(tok, s)
          if !tok.right.is[T.EOF] && tok.right.end <= endPos &&
            exclude.forall(!_.contains(tok.left.start)) &&
            (disallowSingleLineComments || !isSingleLineComment(tok.left)) =>
        if (penaliseNewlinesInsideTokens && tok.leftHasNewline)
          Seq.empty
        else
          s.filterNot(_.isNL)
    }
  }

}
