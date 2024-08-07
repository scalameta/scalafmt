package org.scalafmt.internal

import org.scalafmt.Error.SearchStateExploded
import org.scalafmt.config.FormatEvent.CompleteFormat
import org.scalafmt.config.FormatEvent.Enqueue
import org.scalafmt.config.FormatEvent.Explored
import org.scalafmt.config.FormatEvent.VisitToken
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.TreeOps

import scala.meta.Term
import scala.meta.Type
import scala.meta.tokens.Token

import scala.annotation.tailrec
import scala.collection.mutable

/** Implements best first search to find optimal formatting.
  */
private class BestFirstSearch private (range: Set[Range])(implicit
    val formatOps: FormatOps,
    formatWriter: FormatWriter,
) {
  import BestFirstSearch._
  import LoggerOps._
  import TreeOps._
  import formatOps._

  implicit val stateOrdering: Ordering[State] = State.Ordering

  /** Precomputed table of splits for each token.
    */
  val routes: Array[Seq[Split]] = {
    val router = new Router(formatOps)
    val result = Array.newBuilder[Seq[Split]]
    tokens.foreach(t => result += router.getSplits(t))
    result.result()
  }
  private val noOptZones =
    if (useNoOptZones(initStyle)) getNoOptZones(tokens) else null

  var explored = 0
  var deepestYet = State.start
  val best = mutable.Map.empty[Int, State]
  val visits = new Array[Int](tokens.length)
  var keepSlowStates = !initStyle.runner.optimizer.pruneSlowStates

  /** Returns true if it's OK to skip over state.
    */
  def shouldEnterState(curr: State): Boolean = keepSlowStates ||
    curr.policy.noDequeue ||
    // TODO(olafur) document why/how this optimization works.
    !best.get(curr.depth).exists(_.alwaysBetter(curr))

  private def getBlockCloseToRecurse(ft: FormatToken, stop: Token)(implicit
      style: ScalafmtConfig,
  ): Option[Token] =
    if (style.runner.optimizer.recurseOnBlocks) {
      val prev = tokens.prev(ft)
      getEndOfBlock(prev, false).filter { close =>
        // Block must span at least 3 lines to be worth recursing.
        close != stop && distance(prev.left, close) > style.maxColumn * 3 &&
        extractStatementsIfAny(prev.meta.leftOwner).nonEmpty
      }
    } else None

  private val memo = mutable.Map.empty[Long, State]

  def shortestPathMemo(
      start: State,
      stop: Token,
      depth: Int,
      maxCost: Int,
  ): Option[State] = {
    val key = (start.indentation & 0xffL) | (start.column & 0xffffffL) << 8 |
      (start.depth & 0xffffffffL) << 32
    memo.get(key).orElse {
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
      maxCost: Int = Integer.MAX_VALUE,
  ): State = {
    def newGeneration = new mutable.PriorityQueue[State]()
    var Q = newGeneration
    var generations: List[mutable.PriorityQueue[State]] = Nil
    def addGeneration() = if (Q.nonEmpty) {
      generations = Q :: generations
      Q = newGeneration
    }
    Q += start

    def enqueue(state: State) = Q.enqueue(state)

    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    while (true) {
      val curr = Q.dequeue()
      if (curr.depth >= tokens.length) return curr

      val splitToken = tokens(curr.depth)
      val leftTok = splitToken.left
      if (splitToken.right.start > stop.start && leftTok.start < leftTok.end)
        return curr

      implicit val style = styleMap.at(splitToken)
      import style.runner.optimizer._

      val noOptZone = noOptZones == null || !useNoOptZones ||
        noOptZones.contains(leftTok)

      if (noOptZone || shouldEnterState(curr)) {
        trackState(curr, depth, Q.length)

        if (explored > style.runner.maxStateVisits)
          throw new SearchStateExploded(
            deepestYet,
            "exceeded `runner.maxStateVisits`",
          )

        if (curr.split != null && curr.split.isNL)
          if (
            emptyQueueSpots.contains(curr.depth) ||
            dequeueOnNewStatements && curr.allAltAreNL &&
            !(depth == 0 && noOptZone) &&
            (leftTok.is[Token.KwElse] || statementStarts.contains(curr.depth))
          ) addGeneration()

        val blockClose =
          if (start == curr && 0 != maxCost || !noOptZone) None
          else getBlockCloseToRecurse(splitToken, stop)
        if (blockClose.nonEmpty) blockClose.foreach { end =>
          shortestPathMemo(curr, end, depth + 1, maxCost).foreach(enqueue)
        }
        else if (
          escapeInPathologicalCases && isSeqMulti(routes(curr.depth)) &&
          visits(curr.depth) > maxVisitsPerToken
        ) {
          complete(deepestYet)
          throw new SearchStateExploded(
            deepestYet,
            splitToken,
            "exceeded `runner.optimizer.maxVisitsPerToken`",
          )
        } else {
          val actualSplit = getActiveSplits(curr, maxCost)
          val allAltAreNL = actualSplit.forall(_.isNL)

          var optimalNotFound = true
          actualSplit.foreach { split =>
            val nextState = curr.next(split, allAltAreNL)
            val updateBest = !keepSlowStates && depth == 0 && split.isNL &&
              best.getOrElseUpdate(curr.depth, nextState).eq(nextState)
            style.runner.event(Enqueue(split))
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
                  val overflow = furtherState.appliedPenalty >
                    nextNextState.appliedPenalty
                  if (overflow) enqueue(nextNextState)
                  else {
                    optimalNotFound = false
                    enqueue(furtherState)
                  }
                } else if (!killOnFail && nextState.split.cost <= maxCost)
                  // TODO(olafur) DRY. This solution can still be optimal.
                  enqueue(nextState)
                else // else kill branch
                if (updateBest) best.remove(curr.depth)
              case _ if optimalNotFound && nextState.split.cost <= maxCost =>
                enqueue(nextState)
              case _ => // Kill branch.
                if (updateBest) best.remove(curr.depth)
            }
          }
        }
      }

      if (Q.isEmpty) {
        if (generations.isEmpty) return null

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
    val active = state.policy.execute(Decision(ft, routes(state.depth)))
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

  private def trackState(state: State, depth: Int, queueSize: Int)(implicit
      style: ScalafmtConfig,
  ): Unit = {
    if (state.depth > deepestYet.depth) deepestYet = state
    style.runner.event(VisitToken(tokens(state.depth)))
    visits(state.depth) += 1
    explored += 1
    style.runner.event(Explored(explored, depth, queueSize))
  }

  /** Follow states having single active non-newline split
    */
  @tailrec
  private def traverseSameLine(state: State, depth: Int): State =
    if (state.depth >= tokens.length) state
    else {
      implicit val style = styleMap.at(tokens(state.depth))
      trackState(state, depth, 0)
      val activeSplits = getActiveSplits(state, Int.MaxValue)

      if (!isSeqSingle(activeSplits)) if (activeSplits.isEmpty) null else state // dead end if empty
      else {
        val split = activeSplits.head
        if (split.isNL) state
        else {
          style.runner.event(Enqueue(split))
          val nextState = state.next(split, false)
          traverseSameLine(nextState, depth)
        }
      }
    }

  private def complete(state: State)(implicit style: ScalafmtConfig): Unit =
    style.runner.event(CompleteFormat(explored, state, visits))

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
      complete(state)(initStyle)
      SearchResult(state, reachedEOF = true)
    } else {
      val nextSplits = routes(deepestYet.depth)
      val tok = tokens(deepestYet.depth)
      val splitsAfterPolicy = deepestYet.policy
        .execute(Decision(tok, nextSplits))
      def printSeq(vals: Seq[_]): String = vals.mkString("\n  ", "\n  ", "")
      val msg = s"""|UNABLE TO FORMAT,
                    |tok #${deepestYet.depth}/${tokens.length}: $tok
                    |policies:${printSeq(deepestYet.policy.policies)}
                    |splits (before policy):${printSeq(nextSplits)}
                    |splits (after policy):${printSeq(splitsAfterPolicy)}
                    |""".stripMargin
      if (initStyle.runner.debug) logger.debug(
        s"""|Failed to format
            |$msg""".stripMargin,
      )
      complete(deepestYet)(initStyle)
      SearchResult(deepestYet, reachedEOF = false)
    }
  }
}

case class SearchResult(state: State, reachedEOF: Boolean)

object BestFirstSearch {

  def apply(
      range: Set[Range],
  )(implicit formatOps: FormatOps, formatWriter: FormatWriter): SearchResult =
    new BestFirstSearch(range).getBestPath

  private def getNoOptZones(tokens: FormatTokens) = {
    val result = Set.newBuilder[Token]
    var expire: Token = null
    tokens.foreach {
      case FormatToken(x, _, _) if expire ne null =>
        if (x eq expire) expire = null else result += x
      case FormatToken(t: Token.LeftParen, _, m) if (m.leftOwner match {
            // TODO(olafur) https://github.com/scalameta/scalameta/issues/345
            case lo: Term.ArgClause => !lo.parent.exists(_.is[Term.ApplyInfix])
            case _: Term.Apply => true // legacy: when enclosed in parens
            case _ => false
          }) => expire = tokens.matching(t)
      case FormatToken(t: Token.LeftBrace, _, m) if (m.leftOwner match {
            // Type compounds can be inside defn.defs
            case _: Type.Refine => true
            case _ => false
          }) => expire = tokens.matching(t)
      case _ =>
    }
    result.result()
  }

  private def useNoOptZones(implicit style: ScalafmtConfig): Boolean =
    style.runner.optimizer.disableOptimizationsInsideSensitiveAreas

}
