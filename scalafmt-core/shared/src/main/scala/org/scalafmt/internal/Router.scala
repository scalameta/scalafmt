package org.scalafmt.internal

import org.scalafmt.util._

import scala.meta.tokens.{Token => T}

/** Assigns splits to format tokens.
  */
class Router(implicit formatOps: FormatOps) {

  import LoggerOps._
  import TokenOps._
  import formatOps._
  import tokens._

  /** Assigns possible splits to a FT.
    *
    * The FT can be considered as a node in a graph and the splits as edges.
    * Given a format token (a node in the graph), Route determines which edges
    * lead out from the format token.
    */
  def getSplits(implicit ft: FT): Seq[Split] = {
    implicit val style = styleMap.at(ft)
    import ft._, SplitsBuilder._
    val splitsRaw = lookupAfter.get(left.getClass).flatMap(_.getOpt)
      .orElse(lookupBefore.get(right.getClass).flatMap(_.getOpt))
      .orElse(SplitsBeforeStatement.getOpt)
      .orElse(lookupAfterLowPriority.get(left.getClass).flatMap(_.getOpt))
      .orElse(lookupBeforeLowPriority.get(right.getClass).flatMap(_.getOpt))
      .orElse(SplitsWithOptionalNL.getOpt).getOrElse(Seq(Split(Space, 0)))
      .flatMap(s =>
        if (s.isIgnored) None
        else {
          val penalize = !s.isNL && s.policy.exists {
            case p: PolicyOps.PenalizeAllNewlines => p.failsSyntaxNL(ft)
            case _ => false
          }
          if (penalize) Some(s.withPenalty(1)) else Some(s)
        },
      )
    val splits = optimizationEntities.semicolonAfterStatement(idx)
      .fold(splitsRaw) { xft =>
        import PolicyOps._
        val policy = delayedBreakPolicyFor(xft)(decideNewlinesOnlyAfterToken)
        splitsRaw.map(_.andPolicy(policy))
      }
    require(
      splits.nonEmpty,
      s"""|
          |FT: ${log2(ft)}
          |LO: ${log(ft.leftOwner)}
          |RO: ${log(ft.rightOwner)}
          |""".stripMargin,
    )
    def splitsAsNewlines(splits: Seq[Split]): Seq[Split] = {
      val filtered = Decision.onlyNewlineSplits(splits)
      if (filtered.nonEmpty) filtered else splits.map(_.withMod(Newline))
    }
    ft match {
      case FT(_: T.BOF, _, _) => splits
      case FT(_, _: T.Comment, _) if rhsIsCommentedOutIfComment(ft) =>
        splitsAsNewlines(splits.map(_.withNoIndent))
      case FT(_: T.Comment, _, _) =>
        if (ft.noBreak) splits else splitsAsNewlines(splits)
      case FT(_, _: T.Comment, _) if hasBreakAfterRightBeforeNonComment(ft) =>
        if (ft.noBreak) splits.map(_.withMod(Space))
        else splitsAsNewlines(splits)
      case ft if ft.meta.formatOff && ft.hasBreak => splitsAsNewlines(splits)
      case _ => splits
    }
  }

}
