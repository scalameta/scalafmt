package org.scalafmt.internal

import org.scalafmt.Error.CantFormatFile
import org.scalafmt.ScalaStyle
import org.scalafmt.util.LoggerOps
import LoggerOps._
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.internal.ast.Term
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

/**
  * Implements best first search to find optimal formatting.
  */
class BestFirstSearch(val formatOps: FormatOps, range: Set[Range]) {
  import TreeOps._
  import TokenOps._
  val router = new Router(formatOps)
  import formatOps._

  val maxVisitStates = // For debugging purposes only.
    if (style.debug) 100000 // Unit tests must be < 100k states
    else 10000000

  val doOptimizations = true // For debugging purposes only.

  /**
    * When entering a new statement, clear out search queue.
    */
  val dequeueOnNewStatements = true && doOptimizations

  /**
    * Dequeue on new statements if queue exceeds this size,
    *
    * Overrides [[dequeueOnNewStatements]], appears necessary in cases like
    * JavaLangObject.scala in Scala.js.
    *
    * TODO(olafur) come up with less hacky solution.
    */
  val maxQueueSize = 555

  /**
    * Whether to listen to optimalAt fields in Splits.
    */
  val acceptOptimalAtHints = true && doOptimizations

  /**
    * Do not optimize inside certain areas such as term apply.
    */
  val disableOptimizationsInsideSensitiveAreas = true && doOptimizations

  /**
    * Recursively format { ... } blocks inside no optimization zones.
    *
    * By starting a new search queue, we can perform aggressive optimizations
    * inside optimizations zones.
    */
  val recurseOnBlocks = true && doOptimizations

  val noOptimizations = noOptimizationZones(tree)
  var explored = 0
  var deepestYet = State.start
  var statementCount = 0

  type StateHash = Long

  def isInsideNoOptZone(token: FormatToken): Boolean = {
    !disableOptimizationsInsideSensitiveAreas ||
    noOptimizations.contains(token.left)
  }

  def getLeftLeft(curr: State): Token = {
    tokens(Math.max(0, curr.splits.length - 1)).left
  }

  def shouldRecurseOnBlock(curr: State, stop: Token) = {
    val leftLeft = getLeftLeft(curr)
    val leftLeftOwner = ownersMap(hash(leftLeft))
    val splitToken = tokens(curr.splits.length)
    recurseOnBlocks && isInsideNoOptZone(splitToken) &&
    leftLeft.isInstanceOf[`{`] &&
    matchingParentheses(hash(leftLeft)) != stop && {
      // Block must span at least 3 lines to be worth recursing.
      val close = matchingParentheses(hash(leftLeft))
      // TODO(olafur) magic number
      close.start - leftLeft.end > style.maxColumn * 3
    } && extractStatementsIfAny(leftLeftOwner).nonEmpty
  }

  def provided(formatToken: FormatToken): Split = {
    // TODO(olafur) the indentation is not correctly set.
    val split = Split(Provided(formatToken.between.map(_.code).mkString), 0)
    val result =
      if (formatToken.left.isInstanceOf[`{`])
        split.withIndent(
            Num(2), matchingParentheses(hash(formatToken.left)), Right)
      else split
    result
  }

  def stateColumnKey(state: State): StateHash = {
    state.column << 8 | state.indentation
  }

  def hasReachedEof(state: State): Boolean = {
    explored > maxVisitStates || state.splits.length == tokens.length
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

  /**
    * Runs best first search to find lowest penalty split.
    */
  def shortestPath(start: State,
                   stop: Token,
                   depth: Int = 0,
                   maxCost: Int = Integer.MAX_VALUE)(
      implicit line: sourcecode.Line): State = {
    val Q = new mutable.PriorityQueue[State]()
    var result = start
    Q += start
    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      explored += 1
      if (explored % 10000 == 0 && style.debug) {
        logger.debug(s"Explored $explored, depth=$depth Q.size=${Q.size}")
      }
      if (hasReachedEof(curr) ||
          tokens(curr.splits.length).left.start >= stop.start) {
        result = curr
        Q.dequeueAll
      } else {
        val splitToken = tokens(curr.splits.length)
        if (depth == 0 && curr.splits.length > deepestYet.splits.length) {
          deepestYet = curr
        }

        if (dequeueOnNewStatements &&
            statementStarts.contains(hash(splitToken.left)) &&
            (depth > 0 || !isInsideNoOptZone(splitToken) ||
                Q.size > maxQueueSize) &&
            curr.splits.last.modification.isNewline) {
          Q.dequeueAll
        }

        if (style.debug) {
          Debug.visit(splitToken)
        }
        if (shouldRecurseOnBlock(curr, stop)) {
          val close = matchingParentheses(hash(getLeftLeft(curr)))
          val nextState = shortestPathMemo(
              curr, close, depth = depth + 1, maxCost = maxCost)
          val nextToken = tokens(nextState.splits.length)
          if (nextToken.left == close) {
            Q.enqueue(nextState)
          }
        } else {
          val splits: Seq[Split] =
            if (curr.formatOff) List(provided(splitToken))
            else if (splitToken.inside(range)) router.getSplitsMemo(splitToken)
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
            val nextState = curr.next(style, split, splitToken)
            if (style.debug) {
              Debug.enqueued(split)
            }
            split.optimalAt match {
              case Some(OptimalToken(token, killOnFail))
                  if acceptOptimalAtHints && actualSplit.length > 1 &&
                  split.cost == 0 =>
                val nextNextState =
                  shortestPath(nextState, token, depth + 1, maxCost = 0)(
                      sourcecode.Line.generate)
                if (hasReachedEof(nextNextState) ||
                    (nextNextState.splits.length < tokens.length && tokens(
                            nextNextState.splits.length).left.start >= token.start)) {
                  optimalNotFound = false
                  Q.enqueue(nextNextState)
                } else if (!killOnFail &&
                           nextState.cost - curr.cost <= maxCost) {
                  // TODO(olafur) DRY. This solution can still be optimal.
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

  def getBestPath(): Vector[Split] = {
    var state = shortestPath(State.start, tree.tokens.last)
    if (state.splits.length != tokens.length) {
      val nextSplits = router.getSplits(tokens(deepestYet.splits.length))
      val tok = tokens(deepestYet.splits.length)
      val msg = s"""UNABLE TO FORMAT,
                   |tok=$tok
                   |state.length=${state.splits.length}
                   |toks.length=${tokens.length}
                   |deepestYet.length=${deepestYet.splits.length}
                   |policies=${deepestYet.policy.policies}
                   |nextSplits=$nextSplits
                   |splitsAfterPolicy=${deepestYet.policy
                     .execute(Decision(tok, nextSplits))}
                   |""".stripMargin
      if (style.debug) {
        logger.error(s"""Failed to format
                        |$msg
          """.stripMargin)
        state = deepestYet
      } else {
        throw CantFormatFile(msg)
      }
    }
    if (style.debug) {
      Debug.explored += explored
      Debug.state = state
      Debug.tokens = tokens
    }
    state.splits
  }

}
