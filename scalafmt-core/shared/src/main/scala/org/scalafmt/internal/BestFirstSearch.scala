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
  ): Option[Token] = getEndOfBlock(ft, parensToo = true).collect {
    case close if close.left != stop && {
          // Block must span at least 3 lines to be worth recursing.
          tokens.distance(ft, close) > style.maxColumn * 3
        } => close.left
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
    Q.enqueue(start)

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
            optimizer.dequeueOnNewStatements && !(depth == 0 &&
              noOptZone) && optimizationEntities.statementStarts.contains(idx)
          ) Q.addGeneration()

        val noBlockClose = start == curr && 0 != maxCost || !noOptZone ||
          !optimizer.recurseOnBlocks
        val blockClose =
          if (noBlockClose) None else getBlockCloseToRecurse(splitToken, stop)
        if (blockClose.nonEmpty) blockClose.foreach { end =>
          shortestPathMemo(curr, end, depth + 1, maxCost).foreach(Q.enqueue)
        }
        else {
          if (optimizer.escapeInPathologicalCases && isSeqMulti(routes(idx)))
            stats.explode(splitToken, optimizer.maxVisitsPerToken)(
              stats.visits(idx) > _,
              x => s"exceeded `runner.optimizer.maxVisitsPerToken`=$x",
            )

          val actualSplit = getActiveSplits(splitToken, curr, maxCost)

          var optimalNotFound = true
          val handleOptimalTokens = optimizer.acceptOptimalAtHints &&
            depth < optimizer.maxDepth && actualSplit.lengthCompare(1) > 0

          def processNextState(implicit nextState: State): Unit = {
            val split = nextState.split
            val cost = split.costWithPenalty
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
                stats.updateBest(nextState, stateToQueue)
                Q.enqueue(stateToQueue)
              }
            }
          }

          actualSplit.foreach { split =>
            if (optimalNotFound) processNextState(getNext(curr, split))
            else sendEvent(split)
          }
        }
      }
    }

    Left(deepestState)
  }

  private def sendEvent(split: Split): Unit = initStyle.runner
    .event(FormatEvent.Enqueue(split))

  private def getNext(state: State, split: Split)(implicit
      style: ScalafmtConfig,
  ): State = {
    sendEvent(split)
    state.next(split)
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
    def checkPenalty(state: State, orElse: => Either[State, State]) =
      if (state.appliedPenalty > nextNextState.appliedPenalty)
        Left(nextNextState)
      else orElse
    traverseSameLine(nextNextState) match {
      case x @ Left(s) =>
        if (s eq null) Left(killOnFail(opt, nextNextState))
        else checkPenalty(s, x)
      case x @ Right(s) => checkPenalty(s, if (opt.recurseOnly) Left(s) else x)
    }
  }

  private def getActiveSplits(ft: FormatToken, state: State, maxCost: Int)(
      implicit Q: StateQueue,
  ): Seq[Split] = {
    stats.trackState(state)
    val useProvided = ft.meta.formatOff || !ft.inside(range)
    val active = state.policy.execute(Decision(ft, routes(state.depth)))
      .filter(x => x.isActive && x.costWithPenalty <= maxCost)
    val splits =
      if (useProvided && active.nonEmpty) {
        val isNL = ft.hasBreak
        val mod = Provided(ft)
        active.map { x =>
          val penalty = if (x.isNL == isNL) 0 else Constants.ShouldBeNewline
          x.withMod(mod).withPenalty(penalty)
        }
      } else active
    splits.sortBy(_.costWithPenalty)
  }

  private def stateAsOptimal(state: State, splits: Seq[Split]) = {
    val isOptimal = splits.exists { s =>
      s.isNL && s.penalty < Constants.ShouldBeNewline
    } || tokens.isRightCommentWithBreak(tokens(state.depth))
    if (isOptimal) Some(state) else None
  }

  /** Follow states having single active non-newline split
    */
  @tailrec
  private def traverseSameLine(
      state: State,
  )(implicit queue: StateQueue): Either[State, State] =
    if (state.depth >= tokens.length) Right(state)
    else {
      val splitToken = tokens(state.depth)
      implicit val style: ScalafmtConfig = styleMap.at(splitToken)
      getActiveSplits(splitToken, state, Int.MaxValue) match {
        case Seq() => Left(null) // dead end if empty
        case Seq(split) =>
          if (split.isNL) Right(state)
          else traverseSameLine(getNext(state, split))
        case ss => stateAsOptimal(state, ss).toRight(state)
      }
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

  private def getNoOptZones(tokens: FormatTokens)(implicit styleMap: StyleMap) = {
    val result = Set.newBuilder[Token]
    var expire: Token = null
    tokens.foreach {
      case FormatToken(x, _, _) if expire ne null =>
        if (x eq expire) expire = null else result += x
      case FormatToken(t: Token.LeftParen, _, m) if (m.leftOwner match {
            case lo: Term.ArgClause => !lo.parent.is[Term.ApplyInfix] &&
              !styleMap.at(t).newlines.keep
            case _: Term.Apply => true // legacy: when enclosed in parens
            case _ => false
          }) => expire = tokens.matching(t).left
      case FormatToken(t: Token.LeftBrace, _, m) if (m.leftOwner match {
            // Type compounds can be inside defn.defs
            case lo: meta.Stat.Block => lo.parent.is[Type.Refine]
            case _: Type.Refine => true
            case _ => false
          }) => expire = tokens.matching(t).left
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

    private def updateBestImpl(state: State): Boolean = state.split.isNL &&
      !state.policy
        .exists(_.exists(_.isInstanceOf[PolicyOps.SingleLineBlock])) &&
      (best.getOrElseUpdate(state.prev.depth, state) eq state)

    def updateBest(state: State, furtherState: State)(implicit
        Q: StateQueue,
    ): Boolean = (pruneSlowStates ne ScalafmtOptimizer.PruneSlowStates.No) &&
      Q.nested == 0 && {
        if (state ne furtherState) updateBestImpl(furtherState)
        updateBestImpl(state)
      }

    def checkExplored(
        ft: FormatToken,
    )(implicit formatWriter: FormatWriter): Unit = explode(
      ft,
      runner.getMaxStateVisits,
    )(explored > _, x => s"exceeded `runner.maxStateVisits`=$x")

    def explode[A](ft: FormatToken, value: A)(
        cond: A => Boolean,
        msg: A => String,
    )(implicit formatWriter: FormatWriter): Unit = if (cond(value)) {
      complete(deepestYet)
      throw new Error.SearchStateExploded(deepestYet, ft, msg(value))
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
