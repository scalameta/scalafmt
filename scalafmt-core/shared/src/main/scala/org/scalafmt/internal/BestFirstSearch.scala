package org.scalafmt.internal

import scala.collection.mutable
import scala.meta.tokens.Token

import org.scalafmt.Error.SearchStateExploded
import org.scalafmt.config.FormatEvent.CompleteFormat
import org.scalafmt.config.FormatEvent.Enqueue
import org.scalafmt.config.FormatEvent.Explored
import org.scalafmt.config.FormatEvent.VisitToken
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.ExpiresOn.Right
import org.scalafmt.internal.Length.Num
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps

/**
  * Implements best first search to find optimal formatting.
  */
class BestFirstSearch(
    val formatOps: FormatOps,
    range: Set[Range],
    formatWriter: FormatWriter
) {
  import Token._

  import LoggerOps._
  import TokenOps._
  import TreeOps._
  import formatOps._
  import formatOps.runner.optimizer._

  implicit val stateOrdering = State.Ordering

  /**
    * Precomputed table of splits for each token.
    */
  val routes: Array[Seq[Split]] = {
    val router = new Router(formatOps)
    val result = Array.newBuilder[Seq[Split]]
    tokens.foreach { t => result += router.getSplitsMemo(t) }
    result.result()
  }
  val noOptimizations = noOptimizationZones(tree)
  var explored = 0
  var deepestYet = State.start
  var statementCount = 0
  val best = mutable.Map.empty[Token, State]
  var pathologicalEscapes = 0
  val visits = mutable.Map.empty[FormatToken, Int].withDefaultValue(0)

  type StateHash = Long

  def isInsideNoOptZone(token: FormatToken): Boolean = {
    !disableOptimizationsInsideSensitiveAreas ||
    noOptimizations.contains(token.left)
  }

  /**
    * Returns true if it's OK to skip over state.
    */
  def shouldEnterState(curr: State): Boolean = {
    val splitToken = tokens(curr.depth)
    val insideOptimizationZone =
      curr.policy.noDequeue || isInsideNoOptZone(splitToken)
    def hasBestSolution = !pruneSlowStates || insideOptimizationZone || {
      // TODO(olafur) document why/how this optimization works.
      val result = !best.get(splitToken.left).exists(_.alwaysBetter(curr))
      result
    }
    hasBestSolution
  }

  def shouldRecurseOnBlock(
      ft: FormatToken,
      stop: Token,
      style: ScalafmtConfig
  ): Option[Token] =
    if (!recurseOnBlocks || !isInsideNoOptZone(ft)) None
    else {
      val left = tokens(ft, -1)
      if (!left.left.is[LeftBrace]) None
      else {
        val close = matching(left.left)
        // Block must span at least 3 lines to be worth recursing.
        val ok = close != stop &&
          distance(left.left, close) > style.maxColumn * 3 &&
          extractStatementsIfAny(left.meta.leftOwner).nonEmpty
        if (ok) Some(close) else None
      }
    }

  def provided(formatToken: FormatToken): Split = {
    // TODO(olafur) the indentation is not correctly set.
    val split = Split(Provided(formatToken.between.map(_.syntax).mkString), 0)
    val result =
      if (formatToken.left.is[LeftBrace])
        split.withIndent(Num(2), matching(formatToken.left), Right)
      else split
    result
  }

  def stateColumnKey(state: State): StateHash = {
    state.column << 8 | state.indentation
  }

  def hasReachedEof(state: State, stop: Token): Boolean =
    state.depth >= tokens.length || {
      val token = tokens(state.depth)
      token.left.start >= stop.start && token.left.end > token.left.start
    }

  val memo = mutable.Map.empty[(Int, StateHash), State]

  def shortestPathMemo(start: State, stop: Token, depth: Int, maxCost: Int)(
      implicit line: sourcecode.Line
  ): Option[State] = {
    val key = (start.depth, stateColumnKey(start))
    val cachedState = memo.get(key)
    if (cachedState.nonEmpty) cachedState
    else {
      // Only update state if it reached stop.
      val nextState = shortestPath(start, stop, depth, maxCost)
      if (tokens(nextState.depth).left != stop) None
      else {
        memo.update(key, nextState)
        Some(nextState)
      }
    }
  }

  /**
    * Runs best first search to find lowest penalty split.
    */
  def shortestPath(
      start: State,
      stop: Token,
      depth: Int = 0,
      maxCost: Int = Integer.MAX_VALUE
  ): State = {
    val Q = new mutable.PriorityQueue[State]()
    var result = start
    Q += start
    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      explored += 1
      runner.eventCallback(Explored(explored, depth, Q.size))
      if (explored > runner.maxStateVisits)
        throw SearchStateExploded(
          deepestYet,
          formatWriter.mkString(deepestYet),
          tokens(deepestYet.depth).left
        )
      if (hasReachedEof(curr, stop)) {
        result = curr
        Q.clear()
      } else if (shouldEnterState(curr)) {
        val splitToken = tokens(curr.depth)
        val style = styleMap.at(splitToken)
        if (curr.depth > deepestYet.depth) {
          deepestYet = curr
        }
        runner.eventCallback(VisitToken(splitToken))
        visits.put(splitToken, visits(splitToken) + 1)

        if (curr.split != null && curr.split.modification.isNewline) {
          val tokenHash = hash(splitToken.left)
          if (emptyQueueSpots.contains(tokenHash) ||
            dequeueOnNewStatements &&
            dequeueSpots.contains(tokenHash) &&
            (depth > 0 || !isInsideNoOptZone(splitToken)))
            Q.clear()
        }

        val blockClose = shouldRecurseOnBlock(splitToken, stop, style)
        if (blockClose.nonEmpty)
          shortestPathMemo(curr, blockClose.get, depth + 1, maxCost)
            .foreach(Q.enqueue(_))
        else if (escapeInPathologicalCases &&
          visits(splitToken) > maxVisitsPerToken) {
          Q.clear()
          best.clear()
          visits.clear()
          runner.eventCallback(CompleteFormat(explored, deepestYet, tokens))
          throw SearchStateExploded(
            deepestYet,
            formatWriter.mkString(deepestYet),
            tokens(deepestYet.depth).left
          )
        } else {

          val splits: Seq[Split] =
            if (curr.formatOff) List(provided(splitToken))
            else if (splitToken.inside(range)) routes(curr.depth)
            else List(provided(splitToken))

          val actualSplit = {
            curr.policy
              .execute(Decision(splitToken, splits))
              .splits
              .sortBy(_.cost)
          }
          var optimalNotFound = true
          actualSplit.foreach { split =>
            val nextState = curr.next(style, split, splitToken)
            if (depth == 0 && split.modification.isNewline &&
              !best.contains(splitToken.left)) {
              best.update(splitToken.left, nextState)
            }
            runner.eventCallback(Enqueue(split))
            split.optimalAt match {
              case Some(OptimalToken(token, killOnFail))
                  if acceptOptimalAtHints && optimalNotFound &&
                    actualSplit.length > 1 && depth < maxDepth &&
                    nextState.split.cost == 0 =>
                val nextNextState =
                  shortestPath(nextState, token, depth + 1, maxCost = 0)
                if (hasReachedEof(nextNextState, token)) {
                  optimalNotFound = false
//                  logger.elem(split, splitToken, formatWriter.mkString(nextNextState.splits), tokens(nextNextState.splits.length))
                  Q.enqueue(nextNextState)
                } else if (!killOnFail &&
                  nextState.cost - curr.cost <= maxCost) {
                  // TODO(olafur) DRY. This solution can still be optimal.
//                  logger.elem(split, splitToken)
                  Q.enqueue(nextState)
                } // else kill branch
              case _
                  if optimalNotFound &&
                    nextState.cost - curr.cost <= maxCost =>
                Q.enqueue(nextState)
              case _ => // Kill branch.
            }
          }
        }
      }
    }
    result
  }

  def getBestPath: SearchResult = {
    val state = shortestPath(State.start, tree.tokens.last)
    if (state.depth == tokens.length) {
      runner.eventCallback(CompleteFormat(explored, state, tokens))
      SearchResult(state, reachedEOF = true)
    } else {
      val nextSplits = routes(deepestYet.depth)
      val tok = tokens(deepestYet.depth)
      val splitsAfterPolicy =
        deepestYet.policy.execute(Decision(tok, nextSplits))
      val msg = s"""UNABLE TO FORMAT,
                   |tok=$tok
                   |state.depth=${state.depth}
                   |toks.length=${tokens.length}
                   |deepestYet.length=${deepestYet.depth}
                   |policies=${deepestYet.policy.policies}
                   |nextSplits=$nextSplits
                   |splitsAfterPolicy=$splitsAfterPolicy""".stripMargin
      if (runner.debug) {
        logger.debug(s"""Failed to format
                        |$msg""".stripMargin)
      }
      runner.eventCallback(CompleteFormat(explored, deepestYet, tokens))
      SearchResult(deepestYet, reachedEOF = false)
    }
  }
}

case class SearchResult(state: State, reachedEOF: Boolean)
