package org.scalafmt.internal

import scala.collection.mutable
import scala.meta.Defn
import scala.meta.tokens.Token

import org.scalafmt.Error.SearchStateExploded
import org.scalafmt.config.FormatEvent.CompleteFormat
import org.scalafmt.config.FormatEvent.Enqueue
import org.scalafmt.config.FormatEvent.Explored
import org.scalafmt.config.FormatEvent.VisitToken
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
    formatWriter: FormatWriter) {
  import Token._

  import LoggerOps._
  import TokenOps._
  import TreeOps._
  import formatOps._
  import formatOps.runner.optimizer._

  /**
    * Precomputed table of splits for each token.
    */
  val routes: Array[Seq[Split]] = {
    val router = new Router(formatOps)
    val result = Array.newBuilder[Seq[Split]]
    tokens.foreach { t =>
      result += router.getSplitsMemo(t)
    }
    result.result()
  }
  val noOptimizations = noOptimizationZones(tree)
  var explored = 0
  var deepestYet = State.start
  var deepestYetSafe = State.start
  var statementCount = 0
  val best = mutable.Map.empty[Token, State]
  var pathologicalEscapes = 0
  val visits = mutable.Map.empty[FormatToken, Int].withDefaultValue(0)

  type StateHash = Long

  def isInsideNoOptZone(token: FormatToken): Boolean = {
    !disableOptimizationsInsideSensitiveAreas ||
    noOptimizations.contains(token.left)
  }

  def getLeftLeft(curr: State): Token = {
    tokens(Math.max(0, curr.splits.length - 1)).left
  }

  /**
    * Returns true if it's OK to skip over state.
    */
  def shouldEnterState(curr: State): Boolean = {
    val splitToken = tokens(curr.splits.length)
    val insideOptimizationZone =
      curr.policy.noDequeue || isInsideNoOptZone(splitToken)
    def hasBestSolution = !pruneSlowStates || insideOptimizationZone || {
      val splitToken = tokens(curr.splits.length)
      // TODO(olafur) document why/how this optimization works.
      val result = !best.get(splitToken.left).exists(_.alwaysBetter(curr))
      result
    }
    hasBestSolution
  }

  def shouldRecurseOnBlock(curr: State, stop: Token) = {
    val leftLeft = getLeftLeft(curr)
    val leftLeftOwner = ownersMap(hash(leftLeft))
    val splitToken = tokens(curr.splits.length)
    val style = styleMap.at(splitToken)
    recurseOnBlocks && isInsideNoOptZone(splitToken) &&
    leftLeft.is[LeftBrace] && matchingParentheses(hash(leftLeft)) != stop && {
      // Block must span at least 3 lines to be worth recursing.
      val close = matchingParentheses(hash(leftLeft))
      distance(leftLeft, close) > style.maxColumn * 3
    } && extractStatementsIfAny(leftLeftOwner).nonEmpty
  }

  def provided(formatToken: FormatToken): Split = {
    // TODO(olafur) the indentation is not correctly set.
    val split = Split(Provided(formatToken.between.map(_.syntax).mkString), 0)
    val result =
      if (formatToken.left.is[LeftBrace])
        split.withIndent(
          Num(2),
          matchingParentheses(hash(formatToken.left)),
          Right)
      else split
    result
  }

  def stateColumnKey(state: State): StateHash = {
    state.column << 8 | state.indentation
  }

  def hasReachedEof(state: State): Boolean = {
    explored > runner.maxStateVisits || state.splits.length == tokens.length
  }

  val memo = mutable.Map.empty[(Int, StateHash), State]

  def shortestPathMemo(start: State, stop: Token, depth: Int, maxCost: Int)(
      implicit line: sourcecode.Line): State = {
    val key = (start.splits.length, stateColumnKey(start))
    val cachedState = memo.get(key)
    cachedState match {
      case Some(state) => state
      case None =>
        // Only update state if it reached stop.
        val nextState = shortestPath(start, stop, depth, maxCost)
        if (tokens(nextState.splits.length).left == stop) {
          memo.update(key, nextState)
        }
        nextState
    }
  }

  def untilNextStatement(state: State, maxDepth: Int): State = {
    var curr = state
    while (!hasReachedEof(curr) && {
        val token = tokens(curr.splits.length).left
        !statementStarts.contains(hash(token)) && {
          parents(owners(token)).length <= maxDepth
        }
      }) {
      val tok = tokens(curr.splits.length)
      curr = State.next(curr, styleMap.at(tok), provided(tok), tok)
    }
    curr
  }

  /**
    * Runs best first search to find lowest penalty split.
    */
  def shortestPath(
      start: State,
      stop: Token,
      depth: Int = 0,
      maxCost: Int = Integer.MAX_VALUE): State = {
    val Q = new PriorityQueue[State]()
    var result = start
    var lastDequeue = start
    Q += start
    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      explored += 1
      runner.eventCallback(Explored(explored, depth, Q.size))
      if (hasReachedEof(curr) || {
          val token = tokens(curr.splits.length)
          // If token is empty we can take one more split before reaching stop.
          token.left.syntax.nonEmpty && token.left.start >= stop.start
        }) {
        result = curr
        Q.dequeueAll
      } else if (shouldEnterState(curr)) {
        val splitToken = tokens(curr.splits.length)
        val style = styleMap.at(splitToken)
        if (curr.splits.length > deepestYet.splits.length) {
          deepestYet = curr
        }
        if (curr.policy.isSafe &&
          curr.splits.length > deepestYetSafe.splits.length) {
          deepestYetSafe = curr
        }
        runner.eventCallback(VisitToken(splitToken))
        visits.put(splitToken, visits(splitToken) + 1)

        def lastWasNewline =
          curr.splits.lastOption.exists(_.modification.isNewline)
        if (dequeueOnNewStatements &&
          dequeueSpots.contains(hash(splitToken.left)) &&
          (depth > 0 || !isInsideNoOptZone(splitToken)) &&
          lastWasNewline) {
          Q.dequeueAll
          if (!isInsideNoOptZone(splitToken) && lastDequeue.policy.isSafe) {
            lastDequeue = curr
          }
        } else if (emptyQueueSpots(hash(splitToken.left)) &&
          lastWasNewline) {
          Q.dequeueAll
        }

        if (shouldRecurseOnBlock(curr, stop)) {
          val close = matchingParentheses(hash(getLeftLeft(curr)))
          val nextState =
            shortestPathMemo(curr, close, depth = depth + 1, maxCost = maxCost)
          val nextToken = tokens(nextState.splits.length)
          if (nextToken.left == close) {
            Q.enqueue(nextState)
          }
        } else if (escapeInPathologicalCases &&
          visits(splitToken) > maxVisitsPerToken) {
          Q.dequeueAll
          best.clear()
          visits.clear()
          runner.eventCallback(CompleteFormat(explored, deepestYet, tokens))
          throw SearchStateExploded(
            deepestYet,
            formatWriter.mkString(deepestYet.splits),
            tokens(deepestYet.splits.length).left)
        } else {

          val splits: Seq[Split] =
            if (curr.formatOff) List(provided(splitToken))
            else if (splitToken.inside(range)) routes(curr.splits.length)
            else List(provided(splitToken))

          val actualSplit = {
            curr.policy
              .execute(Decision(splitToken, splits))
              .splits
              .filter(!_.ignoreIf)
              .sortBy(_.cost)
          }
          var optimalNotFound = true
          actualSplit.foreach { split =>
            val nextState = State.next(curr, style, split, splitToken)
            if (depth == 0 && split.modification.isNewline &&
              !best.contains(splitToken.left)) {
              best.update(splitToken.left, nextState)
            }
            runner.eventCallback(Enqueue(split))
            split.optimalAt match {
              case Some(OptimalToken(token, killOnFail))
                  if acceptOptimalAtHints && optimalNotFound &&
                    actualSplit.length > 1 && depth < maxDepth &&
                    nextState.splits.last.cost == 0 =>
                val nextNextState =
                  shortestPath(nextState, token, depth + 1, maxCost = 0)
                if (hasReachedEof(nextNextState) ||
                  (nextNextState.splits.length < tokens.length && tokens(
                    nextNextState.splits.length).left.start >= token.start)) {
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
    if (state.splits.length == tokens.length) {
      runner.eventCallback(CompleteFormat(explored, state, tokens))
      SearchResult(state.splits, reachedEOF = true)
    } else {
      val nextSplits = routes(deepestYet.splits.length)
      val tok = tokens(deepestYet.splits.length)
      val splitsAfterPolicy =
        deepestYet.policy.execute(Decision(tok, nextSplits))
      val msg = s"""UNABLE TO FORMAT,
                   |tok=$tok
                   |state.length=${state.splits.length}
                   |toks.length=${tokens.length}
                   |deepestYet.length=${deepestYet.splits.length}
                   |policies=${deepestYet.policy.policies}
                   |nextSplits=$nextSplits
                   |splitsAfterPolicy=$splitsAfterPolicy""".stripMargin
      if (runner.debug) {
        logger.debug(s"""Failed to format
                        |$msg""".stripMargin)
      }
      runner.eventCallback(CompleteFormat(explored, deepestYet, tokens))
      SearchResult(deepestYet.splits, reachedEOF = false)
    }
  }
}

case class SearchResult(splits: Vector[Split], reachedEOF: Boolean)
