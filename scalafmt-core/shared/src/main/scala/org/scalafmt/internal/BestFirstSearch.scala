package org.scalafmt.internal

import scala.annotation.tailrec
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
private class BestFirstSearch private (
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
  val best = mutable.Map.empty[Int, State]
  val visits = new Array[Int](tokens.length)
  var keepSlowStates = !pruneSlowStates

  type StateHash = Long

  def isInsideNoOptZone(token: FormatToken): Boolean = {
    !disableOptimizationsInsideSensitiveAreas ||
    noOptimizations.contains(token.left)
  }

  /**
    * Returns true if it's OK to skip over state.
    */
  def shouldEnterState(curr: State): Boolean =
    keepSlowStates || curr.policy.noDequeue ||
      isInsideNoOptZone(tokens(curr.depth)) ||
      // TODO(olafur) document why/how this optimization works.
      !best.get(curr.depth).exists(_.alwaysBetter(curr))

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
      if (null == nextState) None
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
    def newGeneration = new mutable.PriorityQueue[State]()
    var Q = newGeneration
    var generations: List[mutable.PriorityQueue[State]] = Nil
    def addGeneration =
      if (Q.nonEmpty) {
        generations = Q :: generations
        Q = newGeneration
      }
    Q += start

    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    while (true) {
      val curr = Q.dequeue()
      if (curr.depth >= tokens.length)
        return curr

      val splitToken = tokens(curr.depth)
      if (splitToken.left.start >= stop.start &&
        splitToken.left.start < splitToken.left.end) {
        return curr
      }

      if (shouldEnterState(curr)) {
        trackState(curr, depth, Q.length)

        if (explored > runner.maxStateVisits)
          throw SearchStateExploded(
            deepestYet,
            formatWriter.mkString(deepestYet),
            tokens(deepestYet.depth).left
          )

        val style = styleMap.at(splitToken)

        if (curr.split != null && curr.split.modification.isNewline) {
          val tokenHash = hash(splitToken.left)
          if (emptyQueueSpots.contains(tokenHash) ||
            dequeueOnNewStatements &&
            dequeueSpots.contains(tokenHash) &&
            (depth > 0 || !isInsideNoOptZone(splitToken)))
            if (depth == 0) addGeneration
            else Q.clear()
        }

        val blockClose = shouldRecurseOnBlock(splitToken, stop, style)
        if (blockClose.nonEmpty)
          shortestPathMemo(curr, blockClose.get, depth + 1, maxCost)
            .foreach(Q.enqueue(_))
        else if (escapeInPathologicalCases &&
          visits(curr.depth) > maxVisitsPerToken) {
          complete(deepestYet)
          throw SearchStateExploded(
            deepestYet,
            formatWriter.mkString(deepestYet),
            tokens(deepestYet.depth).left
          )
        } else {
          val actualSplit = getActiveSplits(curr)

          var optimalNotFound = true
          actualSplit.foreach { split =>
            val nextState = curr.next(style, split, splitToken)
            val updateBest = !keepSlowStates && depth == 0 &&
              split.modification.isNewline && !best.contains(curr.depth)
            if (updateBest) {
              best.update(curr.depth, nextState)
            }
            runner.event(Enqueue(split))
            split.optimalAt match {
              case Some(OptimalToken(token, killOnFail))
                  if acceptOptimalAtHints && optimalNotFound &&
                    actualSplit.length > 1 && depth < maxDepth &&
                    nextState.split.cost == 0 =>
                val nextNextState =
                  shortestPath(nextState, token, depth + 1, maxCost = 0)
                val furtherState =
                  if (null != nextNextState && style.activeForEdition_2020_03)
                    traverseSameLine(nextNextState, depth)
                  else nextNextState
                if (null != furtherState) {
                  if (furtherState.column > style.maxColumn)
                    Q.enqueue(nextNextState)
                  else {
                    optimalNotFound = false
                    Q.enqueue(furtherState)
                  }
                } else if (!killOnFail &&
                  nextState.cost - curr.cost <= maxCost) {
                  // TODO(olafur) DRY. This solution can still be optimal.
                  Q.enqueue(nextState)
                } else { // else kill branch
                  if (updateBest) best.remove(curr.depth)
                }
              case _
                  if optimalNotFound &&
                    nextState.cost - curr.cost <= maxCost =>
                Q.enqueue(nextState)
              case _ => // Kill branch.
                if (updateBest) best.remove(curr.depth)
            }
          }
        }
      }

      if (Q.isEmpty) {
        if (generations.isEmpty)
          return null

        Q = generations.head
        generations = generations.tail
      }
    }

    // unreachable
    null
  }

  private def getActiveSplits(state: State): Seq[Split] = {
    val ft = tokens(state.depth)
    val useProvided = state.formatOff || !ft.inside(range)
    val splits = if (useProvided) Seq(provided(ft)) else routes(state.depth)

    state.policy
      .execute(Decision(ft, splits))
      .splits
      .filter(_.isActive)
      .sortBy(_.cost)
  }

  private def trackState(state: State, depth: Int, queueSize: Int): Unit = {
    if (state.depth > deepestYet.depth) deepestYet = state
    runner.event(VisitToken(tokens(state.depth)))
    visits(state.depth) += 1
    explored += 1
    runner.event(Explored(explored, depth, queueSize))
  }

  /**
    * Follow states having single active non-newline split
    */
  @tailrec
  private def traverseSameLine(state: State, depth: Int): State =
    if (state.depth >= tokens.length) state
    else {
      trackState(state, depth, 0)
      val activeSplits = getActiveSplits(state)

      if (activeSplits.lengthCompare(1) != 0)
        if (activeSplits.isEmpty) null else state // dead end if empty
      else {
        val split = activeSplits.head
        if (split.modification.isNewline) state
        else {
          runner.event(Enqueue(split))
          val ft = tokens(state.depth)
          val style = styleMap.at(ft)
          val nextState = state.next(style, split, ft)
          traverseSameLine(nextState, depth)
        }
      }
    }

  private def complete(state: State): Unit =
    runner.event(CompleteFormat(explored, state, visits))

  def getBestPath: SearchResult = {
    val state = {
      def run = shortestPath(State.start, tree.tokens.last)
      val state = run
      if (null != state || keepSlowStates) state
      else {
        best.clear()
        keepSlowStates = true
        run
      }
    }
    if (null != state) {
      complete(state)
      SearchResult(state, reachedEOF = true)
    } else {
      val nextSplits = routes(deepestYet.depth)
      val tok = tokens(deepestYet.depth)
      val splitsAfterPolicy =
        deepestYet.policy.execute(Decision(tok, nextSplits))
      val msg = s"""UNABLE TO FORMAT,
                   |tok=$tok
                   |toks.length=${tokens.length}
                   |deepestYet.length=${deepestYet.depth}
                   |policies=${deepestYet.policy.policies}
                   |nextSplits=$nextSplits
                   |splitsAfterPolicy=$splitsAfterPolicy""".stripMargin
      if (runner.debug) {
        logger.debug(s"""Failed to format
                        |$msg""".stripMargin)
      }
      complete(deepestYet)
      SearchResult(deepestYet, reachedEOF = false)
    }
  }
}

case class SearchResult(state: State, reachedEOF: Boolean)

object BestFirstSearch {

  def apply(
      formatOps: FormatOps,
      range: Set[Range],
      formatWriter: FormatWriter
  ): SearchResult =
    new BestFirstSearch(formatOps, range, formatWriter).getBestPath

}
