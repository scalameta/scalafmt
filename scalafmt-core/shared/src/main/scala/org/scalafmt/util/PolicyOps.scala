package org.scalafmt.util

import scala.meta.tokens.{Token => T}

import org.scalafmt.internal.Decision
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Policy

object PolicyOps {

  /**
    * @param noSyntaxNL do not allow newlines in token syntax
    */
  class PenalizeAllNewlines(
      val endPolicy: Policy.End.WithPos,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      ignore: FormatToken => Boolean = _ => false,
      noSyntaxNL: Boolean = false
  )(implicit line: sourcecode.Line)
      extends Policy.Clause {
    override val noDequeue: Boolean = false
    override val f: Policy.Pf = {
      case Decision(ft, s)
          if (penalizeLambdas || !ft.left.is[T.RightArrow]) && !ignore(ft) =>
        if (noSyntaxNL && ft.leftHasNewline) s.map(_.withPenalty(penalty))
        else s.map(x => if (x.isNL) x.withPenalty(penalty) else x)
    }
    override def toString: String = s"PNL:${super.toString}+$penalty"
  }

  object PenalizeAllNewlines {
    def apply(
        expire: T,
        penalty: Int,
        penalizeLambdas: Boolean = true,
        ignore: FormatToken => Boolean = _ => false,
        noSyntaxNL: Boolean = false
    )(implicit line: sourcecode.Line): PenalizeAllNewlines =
      new PenalizeAllNewlines(
        Policy.End.Before(expire),
        penalty,
        penalizeLambdas,
        ignore,
        noSyntaxNL
      )
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
      case Decision(ft, s)
          if !(ft.right.is[T.EOF] || okSLC && isSingleLineComment(ft.left) ||
            exclude.exists(_.contains(ft.left.start))) =>
        if (noSyntaxNL && ft.leftHasNewline) Seq.empty else s.filterNot(_.isNL)
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
