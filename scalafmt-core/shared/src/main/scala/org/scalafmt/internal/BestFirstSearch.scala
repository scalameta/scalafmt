package org.scalafmt.internal

import org.scalafmt.Error
import org.scalafmt.config._
import org.scalafmt.util._

import scala.meta._
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

  implicit val stateOrdering: Ordering[State] = State.Ordering.get(initStyle)

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

  var stats = new StateStats(tokens, initStyle.runner)

  private def getBlockCloseToRecurse(ft: FormatToken, stop: Token)(implicit
      style: ScalafmtConfig,
  ): Option[Token] = getEndOfBlockOrOptionalBraces(ft).flatMap {
    case Left(x) => Some(x)
    case Right(x) =>
      if (extractStatementsIfAny(ft.meta.leftOwner).isEmpty) None else Some(x)
  }.filter { close =>
    // Block must span at least 3 lines to be worth recursing.
    close != stop && distance(ft.left, close) > style.maxColumn * 3
  }

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
      val nextState = shortestPath(start, stop, depth, maxCost).toOption
      nextState.foreach(memo.update(key, _))
      nextState
    }
  }

  /** Runs best first search to find lowest penalty split.
    */
  def shortestPath(
      start: State,
      stop: Token,
      depth: Int = 0,
      maxCost: Int = Integer.MAX_VALUE,
  ): Either[State, State] = {
    implicit val Q: StateQueue = new StateQueue(depth)
    def enqueue(state: State) = Q.enqueue(state)
    enqueue(start)

    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    var deepestState: State = start
    while (!Q.isEmpty()) {
      val curr = Q.dequeue()
      val idx = curr.depth
      if (idx >= tokens.length) return Right(curr)

      val splitToken = tokens(idx)
      val leftTok = splitToken.left
      if (splitToken.right.start > stop.start && leftTok.start < leftTok.end)
        return Right(curr)

      implicit val style = styleMap.at(splitToken)
      import style.runner.optimizer

      if (idx > deepestState.depth) deepestState = curr
      val noOptZone = noOptZones == null || !useNoOptZones ||
        noOptZones.contains(leftTok)

      if (noOptZone || stats.shouldEnterState(curr)) {
        stats.checkExplored(splitToken)

        if (curr.split != null && curr.split.isNL)
          if (
            emptyQueueSpots.contains(idx) ||
            optimizer.dequeueOnNewStatements && curr.allAltAreNL &&
            !(depth == 0 && noOptZone) &&
            (leftTok.is[Token.KwElse] || statementStarts.contains(idx))
          ) Q.addGeneration()

        val noBlockClose = start == curr && 0 != maxCost || !noOptZone ||
          !optimizer.recurseOnBlocks
        val blockClose =
          if (noBlockClose) None else getBlockCloseToRecurse(splitToken, stop)
        if (blockClose.nonEmpty) blockClose.foreach { end =>
          shortestPathMemo(curr, end, depth + 1, maxCost).foreach(enqueue)
        }
        else if (
          optimizer.escapeInPathologicalCases && isSeqMulti(routes(idx)) &&
          stats.visits(idx) > optimizer.maxVisitsPerToken
        ) stats.explode(splitToken)(
          s"exceeded `runner.optimizer.maxVisitsPerToken`=${optimizer.maxVisitsPerToken}",
        )
        else {
          val actualSplit = getActiveSplits(splitToken, curr, maxCost)
          val allAltAreNL = actualSplit.forall(_.isNL)

          var optimalNotFound = true
          val handleOptimalTokens = optimizer.acceptOptimalAtHints &&
            depth < optimizer.maxDepth && actualSplit.lengthCompare(1) > 0

          def processNextState(implicit nextState: State): Unit = {
            val split = nextState.split
            val cost = split.cost
            if (cost <= maxCost) {
              val stateToQueue = split.optimalAt match {
                case Some(opt) if handleOptimalTokens =>
                  if (cost > 0) killOnFail(opt)
                  else processOptimalToken(opt) match {
                    case Left(x) => x
                    case Right(x) => optimalNotFound = false; x
                  }
                case _ => nextState
              }
              if (null ne stateToQueue) {
                if (depth == 0) stats.updateBest(nextState)
                enqueue(stateToQueue)
              }
            }
          }

          actualSplit.foreach { split =>
            if (optimalNotFound)
              processNextState(getNext(curr, split, allAltAreNL))
            else sendEvent(split)
          }
        }
      }
    }

    Left(deepestState)
  }

  private def sendEvent(split: Split): Unit = initStyle.runner
    .event(FormatEvent.Enqueue(split))

  private def getNext(state: State, split: Split, allAltAreNL: Boolean)(implicit
      style: ScalafmtConfig,
  ): State = {
    sendEvent(split)
    state.next(split, nextAllAltAreNL = allAltAreNL)
  }

  private def killOnFail(opt: OptimalToken, nextNextState: State = null)(
      implicit nextState: State,
  ): State = {
    val kill = opt.killOnFail || hasSlbAfter(nextState) {
      if (
        (null ne nextNextState) &&
        nextNextState.appliedPenalty > nextState.prev.appliedPenalty
      ) tokens(nextNextState.depth)
      else tokens(opt.token)
    }
    if (kill) null else nextState
  }

  private def processOptimalToken(
      opt: OptimalToken,
  )(implicit nextState: State, queue: StateQueue): Either[State, State] = {
    val nextNextState =
      if (opt.token.end <= tokens(nextState.depth).left.end) nextState
      else {
        val res =
          shortestPath(nextState, opt.token, queue.nested + 1, maxCost = 0)
        res match {
          case Right(x) => x
          case Left(x) => return Left(killOnFail(opt, x))
        }
      }
    val furtherState = traverseSameLine(nextNextState)
    if (null eq furtherState) Left(killOnFail(opt, nextNextState))
    else if (furtherState.appliedPenalty > nextNextState.appliedPenalty)
      Left(nextNextState)
    else Either.cond(!opt.recurseOnly, furtherState, furtherState)
  }

  private def getActiveSplits(ft: FormatToken, state: State, maxCost: Int)(
      implicit Q: StateQueue,
  ): Seq[Split] = {
    stats.trackState(state)
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

  /** Follow states having single active non-newline split
    */
  @tailrec
  private def traverseSameLine(
      state: State,
  )(implicit queue: StateQueue): State =
    if (state.depth >= tokens.length) state
    else {
      val splitToken = tokens(state.depth)
      implicit val style: ScalafmtConfig = styleMap.at(splitToken)
      getActiveSplits(splitToken, state, Int.MaxValue) match {
        case Seq() => null // dead end if empty
        case Seq(split) =>
          if (split.isNL) state
          else {
            implicit val nextState: State =
              getNext(state, split, allAltAreNL = false)
            traverseSameLine(nextState)
          }
        case ss
            if state.appliedPenalty == 0 &&
              RightParenOrBracket(splitToken.right) =>
          traverseSameLineZeroCost(ss.filter(_.cost == 0), state)
        case _ => state
      }
    }

  @tailrec
  private def traverseSameLineZeroCost(
      splits: Seq[Split],
      state: State,
  )(implicit style: ScalafmtConfig, queue: StateQueue): State = splits match {
    case Seq(split) if !split.isNL =>
      val nextState: State = getNext(state, split, allAltAreNL = false)
      if (nextState.split.cost > 0) state
      else if (nextState.depth >= tokens.length) nextState
      else {
        val nextToken = tokens(nextState.depth)
        if (RightParenOrBracket(nextToken.right)) {
          implicit val style: ScalafmtConfig = styleMap.at(nextToken)
          val nextSplits = getActiveSplits(nextToken, nextState, maxCost = 0)
          traverseSameLineZeroCost(nextSplits, nextState)
        } else nextState
      }
    case _ => state
  }

  def getBestPath: SearchResult = {
    initStyle.runner.event(FormatEvent.Routes(routes))
    val state = {
      val endToken = topSourceTree.tokens.last
      def run = shortestPath(State.start, endToken)
      run.getOrElse {
        stats.retry.flatMap { x =>
          stats = x
          run.toOption
        }.orNull
      }
    }
    if (null != state) {
      stats.complete(state)
      SearchResult(state, reachedEOF = true)
    } else {
      val deepestYet = stats.deepestYet
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
      stats.complete(deepestYet)
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

  private def hasSlbAfter(state: State)(ft: FormatToken): Boolean = state.policy
    .exists(_.appliesUntil(ft)(_.isInstanceOf[PolicyOps.SingleLineBlock]))

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

  class StateQueue(val nested: Int)(implicit stateOrdering: Ordering[State]) {
    private def newGeneration = new mutable.PriorityQueue[State]()
    var generation: mutable.PriorityQueue[State] = newGeneration
    var generations: List[mutable.PriorityQueue[State]] = Nil

    def addGeneration(): Unit = if (generation.nonEmpty) {
      generations = generation :: generations
      generation = newGeneration
    }

    def dequeue(): State = generation.dequeue()
    def enqueue(state: State): Unit = generation.enqueue(state)
    def length: Int = generation.length
    @tailrec
    final def isEmpty(): Boolean = generation.isEmpty && {
      generations match {
        case head :: tail =>
          generation = head
          generations = tail
          isEmpty()
        case _ => true
      }
    }
  }

  class StateStats private (
      tokens: FormatTokens,
      runner: ScalafmtRunner,
      pruneSlowStates: ScalafmtOptimizer.PruneSlowStates,
  ) {
    var explored = 0
    var deepestYet = State.start
    val best = mutable.Map.empty[Int, State]
    val visits = new Array[Int](tokens.length)

    def this(tokens: FormatTokens, runner: ScalafmtRunner) =
      this(tokens, runner, runner.optimizer.pruneSlowStates)

    /** Returns true if it's OK to skip over state.
      */
    def shouldEnterState(state: State): Boolean = state.policy.noDequeue ||
      (pruneSlowStates eq ScalafmtOptimizer.PruneSlowStates.No) ||
      // TODO(olafur) document why/how this optimization works.
      best.get(state.depth).forall(state.possiblyBetter)

    def trackState(state: State)(implicit Q: StateQueue): Unit = {
      val idx = state.depth
      if (idx > deepestYet.depth) deepestYet = state
      runner.event(FormatEvent.VisitToken(tokens(idx)))
      visits(idx) += 1
      explored += 1
      runner.event(FormatEvent.Explored(explored, Q.nested, Q.length))
    }

    def updateBest(state: State): Boolean =
      (pruneSlowStates ne ScalafmtOptimizer.PruneSlowStates.No) &&
        state.split.isNL &&
        !state.policy
          .exists(_.exists(_.isInstanceOf[PolicyOps.SingleLineBlock])) &&
        (best.getOrElseUpdate(state.prev.depth, state) eq state)

    def checkExplored(ft: FormatToken)(implicit
        formatWriter: FormatWriter,
    ): Unit = if (explored > runner.maxStateVisits)
      explode(ft)(s"exceeded `runner.maxStateVisits`=${runner.maxStateVisits}")

    def explode(
        ft: FormatToken,
    )(msg: String)(implicit formatWriter: FormatWriter): Unit = {
      complete(deepestYet)
      throw new Error.SearchStateExploded(deepestYet, ft, msg)
    }

    def complete(state: State): Unit = runner
      .event(FormatEvent.CompleteFormat(explored, state, visits, best))

    def retry: Option[StateStats] = {
      val ok = best.nonEmpty &&
        (pruneSlowStates eq ScalafmtOptimizer.PruneSlowStates.Yes)
      if (ok) Some(
        new StateStats(tokens, runner, ScalafmtOptimizer.PruneSlowStates.No),
      )
      else None
    }
  }

}
