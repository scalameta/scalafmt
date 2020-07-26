package org.scalafmt.util

import scala.meta.tokens.{Token => T}

import org.scalafmt.internal.Decision
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Policy

object PolicyOps {

  /**
    * @param noSyntaxNL do not allow newlines in token syntax
    */
  case class PenalizeAllNewlines(
      expire: T,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      ignore: FormatToken => Boolean = _ => false,
      noSyntaxNL: Boolean = false
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
                (noSyntaxNL && tok.leftHasNewline) =>
            split.withPenalty(penalty)
          case x => x
        }
    }
    override def toString: String = s"PNL:${super.toString}+$penalty"
  }

  /**
    * Forces all splits up to including expire to be on a single line.
    * @param okSLC if true, allow single-line comments
    * @param noSyntaxNL if false, allow newlines in token syntax
    */
  class SingleLineBlock(
      val endPolicy: Policy.End.WithPos,
      exclude: Set[Range] = Set.empty,
      okSLC: Boolean = false,
      noSyntaxNL: Boolean = false
  )(implicit line: sourcecode.Line)
      extends Policy.Clause {
    import TokenOps.isSingleLineComment
    override val noDequeue: Boolean = true
    override def toString: String =
      "SLB:" + super.toString + {
        if (exclude.isEmpty) ""
        else exclude.map(x => s"${x.start}:${x.end}").mkString("^{", ",", "}")
      }
    override val f: Policy.Pf = {
      case Decision(tok, s)
          if !tok.right.is[T.EOF] &&
            exclude.forall(!_.contains(tok.left.start)) &&
            !(okSLC && isSingleLineComment(tok.left)) =>
        if (noSyntaxNL && tok.leftHasNewline) Seq.empty else s.filterNot(_.isNL)
    }
  }

  object SingleLineBlock {

    def apply(
        expire: T,
        exclude: Set[Range] = Set.empty,
        okSLC: Boolean = false,
        noSyntaxNL: Boolean = false
    )(implicit line: sourcecode.Line): SingleLineBlock =
      new SingleLineBlock(Policy.End.On(expire), exclude, okSLC, noSyntaxNL)

  }

}
