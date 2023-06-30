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
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps

/** Implements best first search to find optimal formatting.
  */
private class BestFirstSearch private (
    range: Set[Range],
    formatWriter: FormatWriter
)(implicit val formatOps: FormatOps) {
  import LoggerOps._
  import TokenOps._
  import TreeOps._
  import formatOps._
  import formatOps.runner.optimizer._

  implicit val stateOrdering: Ordering[State] = State.Ordering
  implicit val tokens: FormatTokens = formatOps.tokens

  /** Precomputed table of splits for each token.
    */
  val routes: Array[Seq[Split]] = {
    val router = new Router(formatOps)
    val result = Array.newBuilder[Seq[Split]]
    tokens.foreach { t => result += router.getSplits(t) }
    result.result()
  }
  val noOptimizations = noOptimizationZones()
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

  /** Returns true if it's OK to skip over state.
    */
  def shouldEnterState(curr: State): Boolean =
    keepSlowStates || curr.policy.noDequeue ||
      isInsideNoOptZone(tokens(curr.depth)) ||
      // TODO(olafur) document why/how this optimization works.
      !best.get(curr.depth).exists(_.alwaysBetter(curr))

  def shouldRecurseOnBlock(ft: FormatToken, stop: Token)(implicit
      style: ScalafmtConfig
  ): Option[Token] =
    if (!recurseOnBlocks || !isInsideNoOptZone(ft)) None
    else {
      val left = tokens(ft, -1)
      val closeOpt = formatOps.getEndOfBlock(left, false)
      closeOpt.filter(close =>
        // Block must span at least 3 lines to be worth recursing.
        close != stop &&
          distance(left.left, close) > style.maxColumn * 3 &&
          extractStatementsIfAny(left.meta.leftOwner).nonEmpty
      )
    }

  def stateColumnKey(state: State): StateHash = {
    state.column << 8 | state.indentation
  }

  val memo = mutable.Map.empty[(Int, StateHash), State]

  def shortestPathMemo(
      start: State,
      stop: Token,
      depth: Int,
      maxCost: Int
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

  /** Runs best first search to find lowest penalty split.
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
    def addGeneration() =
      if (Q.nonEmpty) {
        generations = Q :: generations
        Q = newGeneration
      }
    Q += start

    def enqueue(state: State) = {
      Q.enqueue(state)
    }

    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    while (true) {
      val curr = Q.dequeue()
      if (curr.depth >= tokens.length)
        return curr

      val splitToken = tokens(curr.depth)
      val leftTok = splitToken.left
      if (
        splitToken.right.start > stop.start &&
        leftTok.start < leftTok.end
      ) {
        return curr
      }

      if (shouldEnterState(curr)) {
        trackState(curr, depth, Q.length)

        if (explored > runner.maxStateVisits)
          throw SearchStateExploded(
            deepestYet,
            formatWriter.mkString(deepestYet),
            tokens(deepestYet.depth)
          )

        implicit val style = styleMap.at(splitToken)

        if (curr.split != null && curr.split.isNL) {
          val tokenHash = hash(leftTok)
          if (
            emptyQueueSpots.contains(tokenHash) ||
            dequeueOnNewStatements && curr.allAltAreNL &&
            (leftTok.is[Token.KwElse] || statementStarts.contains(tokenHash)) &&
            (depth > 0 || !isInsideNoOptZone(splitToken))
          )
            addGeneration()
        }

        val blockClose =
          if (start.eq(curr) && 0 != maxCost) None
          else shouldRecurseOnBlock(splitToken, stop)
        if (blockClose.nonEmpty)
          blockClose.foreach { end =>
            shortestPathMemo(curr, end, depth + 1, maxCost).foreach(enqueue)
          }
        else if (
          escapeInPathologicalCases &&
          isSeqMulti(routes(curr.depth)) &&
          visits(curr.depth) > maxVisitsPerToken
        ) {
          complete(deepestYet)
          throw SearchStateExploded(
            deepestYet,
            formatWriter.mkString(deepestYet),
            splitToken
          )
        } else {
          val actualSplit = getActiveSplits(curr, maxCost)
          val allAltAreNL = actualSplit.forall(_.isNL)

          var optimalNotFound = true
          actualSplit.foreach { split =>
            val nextState = curr.next(split, allAltAreNL)
            val updateBest = !keepSlowStates && depth == 0 &&
              split.isNL && !best.contains(curr.depth)
            if (updateBest) {
              best.update(curr.depth, nextState)
            }
            runner.event(Enqueue(split))
            split.optimalAt match {
              case Some(OptimalToken(token, killOnFail))
                  if acceptOptimalAtHints && optimalNotFound &&
                    actualSplit.lengthCompare(1) > 0 && depth < maxDepth &&
                    nextState.split.cost == 0 =>
                val nextNextState =
                  shortestPath(nextState, token, depth + 1, maxCost = 0)
                val furtherState =
                  if (null == nextNextState) null
                  else traverseSameLine(nextNextState, depth)
                if (null != furtherState) {
                  val overflow =
                    furtherState.appliedPenalty > nextNextState.appliedPenalty
                  if (overflow)
                    enqueue(nextNextState)
                  else {
                    optimalNotFound = false
                    enqueue(furtherState)
                  }
                } else if (
                  !killOnFail &&
                  nextState.cost - curr.cost <= maxCost
                ) {
                  // TODO(olafur) DRY. This solution can still be optimal.
                  enqueue(nextState)
                } else { // else kill branch
                  if (updateBest) best.remove(curr.depth)
                }
              case _
                  if optimalNotFound &&
                    nextState.cost - curr.cost <= maxCost =>
                enqueue(nextState)
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

  private def getActiveSplits(state: State, maxCost: Int): Seq[Split] = {
    val ft = tokens(state.depth)
    val useProvided = ft.meta.formatOff || !ft.inside(range)
    val active = state.policy
      .execute(Decision(ft, routes(state.depth)))
      .splits
      .filter(x => x.isActive && x.cost <= maxCost)
    val splits =
      if (useProvided && active.nonEmpty) {
        val isNL = ft.hasBreak
        val mod = Provided(ft)
        active.map { x =>
          val penalty = if (x.isNL == isNL) 0 else Constants.ShouldBeNewline
          x.withMod(mod).withPenalty(penalty)
        }
      } else active
    splits.sortBy(_.cost)
  }

  private def trackState(state: State, depth: Int, queueSize: Int): Unit = {
    if (state.depth > deepestYet.depth) deepestYet = state
    runner.event(VisitToken(tokens(state.depth)))
    visits(state.depth) += 1
    explored += 1
    runner.event(Explored(explored, depth, queueSize))
  }

  /** Follow states having single active non-newline split
    */
  @tailrec
  private def traverseSameLine(state: State, depth: Int): State =
    if (state.depth >= tokens.length) state
    else {
      trackState(state, depth, 0)
      val activeSplits = getActiveSplits(state, Int.MaxValue)

      if (!isSeqSingle(activeSplits))
        if (activeSplits.isEmpty) null else state // dead end if empty
      else {
        val split = activeSplits.head
        if (split.isNL) state
        else {
          runner.event(Enqueue(split))
          implicit val style = styleMap.at(tokens(state.depth))
          val nextState = state.next(split, false)
          traverseSameLine(nextState, depth)
        }
      }
    }

  private def complete(state: State): Unit =
    runner.event(CompleteFormat(explored, state, visits))

  def getBestPath: SearchResult = {
    val state = {
      def run = shortestPath(State.start, topSourceTree.tokens.last)
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
    new BestFirstSearch(range, formatWriter)(formatOps).getBestPath

}
