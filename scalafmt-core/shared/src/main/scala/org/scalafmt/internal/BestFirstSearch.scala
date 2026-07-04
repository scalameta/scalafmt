package org.scalafmt.internal

import org.scalafmt.Error
import org.scalafmt.config._
import org.scalafmt.util._

import scala.meta._
import scala.meta.tokens.{Token => T}

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
    val router = new Router
    val result = new Array[Seq[Split]](tokens.length)
    var i = 0
    while (i < result.length) {
      result(i) = router.getSplits(tokens(i))
      i += 1
    }
    result
  }
  private val noOptZones =
    if (useNoOptZones(initStyle)) getNoOptZones(tokens) else null

  var stats = new StateStats(tokens, initStyle.runner)

  private def getBlockCloseToRecurse(ft: FT, indent: Int)(implicit
      style: ScalafmtConfig,
  ): Int = TokenOps.getEndOfBlock(ft, parens = true) match {
    case (close, _) if (ft.leftOwner match {
          case Term.Block(_ :: tail) if tail.nonEmpty =>
            indent + tokens.width(ft, close) > style.maxColumn
          // Block must span at least 3 lines to be worth recursing.
          case _ => indent + tokens.width(ft, close) > style.maxColumn * 3
        }) => close.idx
    case _ => -1
  }

  // LongMap: primitive Long key, no boxing on get/update in the search recursion
  private val memo = mutable.LongMap.empty[State]
  private val slbMemo = mutable.LongMap.empty[State]

  // out-param for getActiveSplits: number of valid splits in the returned array
  // (avoids a per-state `(Array[Split], Int)` tuple). Single-threaded search, so
  // a field is safe; every caller reads it into a local right after the call.
  private[this] var activeSplitsCount: Int = 0

  // Left(null) is a payload-less dead end; intern one instance to reuse
  private[this] val deadEnd: Either[State, State] = Left(null)
  private def leftState(s: State): Either[State, State] =
    if (s eq null) deadEnd else Left(s)

  def shortestPathMemo(
      start: State,
      stop: Int,
      depth: Int,
      isOpt: Boolean,
  ): State = {
    val key = start.indentation & 0xffL | (start.column & 0xffffffL) << 8 |
      (start.depth & 0xffffffffL) << 32
    def orElse(hadSlb: Boolean) = {
      val res = shortestPath(start, stop, depth, isOpt) match {
        case Right(x) => x
        case _ => State.start
      }
      val useSlb = if (res eq State.start) hadSlb else res.terminal()
      val map = if (useSlb) slbMemo else memo
      map.update(key, res)
      res
    }

    memo.getOrElse(
      key,
      if (isOpt) State.start // we wouldn't recurse unless the span was large
      else if (!start.terminal()) orElse(hadSlb = false)
      else slbMemo.getOrElse(key, orElse(hadSlb = true)),
    )
  }

  /** Runs best first search to find lowest penalty split.
    */
  def shortestPath(
      start: State,
      stop: Int,
      depth: Int = 0,
      isOpt: Boolean = false,
      isKillOnFail: Boolean = true,
  ): Either[State, State] = {
    implicit val Q: StateQueue = new StateQueue(depth)
    Q.enqueue(start)

    val maxCost = if (isOpt) 0 else Int.MaxValue
    var deepestState: State = start
    var preForkState: State = start
    var preFork = !isKillOnFail
    val activeSplitsFilter: Split => Boolean =
      if (isOpt) s => if (s.noCost) true else { preFork = false; false }
      else _ => true

    var curr: State = null
    while ({
      curr = Q.dequeue()
      null ne curr
    }) {
      val idx = curr.depth
      if (idx >= tokens.length) return Right(curr)

      val splitToken = tokens(idx)
      if (idx >= stop && !splitToken.left.isEmpty) return Right(curr)

      implicit val style = styleMap.at(splitToken)
      import style.runner.optimizer

      if (idx > deepestState.depth) deepestState = curr
      // 0 = absent (no block here), 1 = Some(false), 2 = Some(true)/no-opt
      val noOptZoneCode: Int =
        if (noOptZones == null || !useNoOptZones) 2 else noOptZones(idx)
      val noOptZone = noOptZoneCode == 2

      if (noOptZone || stats.shouldEnterState(curr)) {
        if (Q.generation.nonEmpty) preFork = false
        else if (preFork) preForkState = curr

        stats.checkExplored(splitToken)

        if (curr.split != null && curr.split.isNL)
          if (
            emptyQueueSpots.contains(idx) ||
            optimizer.dequeueOnNewStatements && !(depth == 0 && noOptZone) &&
            optimizationEntities.isStatementStart(idx)
          ) {
            preFork = false
            Q.addGeneration()
          }

        val noBlockClose = start == curr && !isOpt || noOptZoneCode == 0 ||
          !optimizer.recurseOnBlocks
        val blockCloseState =
          if (noBlockClose) null
          else {
            val close = getBlockCloseToRecurse(splitToken, curr.indentation)
            if (close < 0) null
            else shortestPathMemo(curr, close, depth + 1, isOpt)
          }
        if (blockCloseState ne null) {
          if (blockCloseState ne State.start) Q.enqueue(blockCloseState)
        } else {
          if (optimizer.escapeInPathologicalCases && isSeqMulti(routes(idx)))
            stats.explode(splitToken, optimizer.maxVisitsPerToken)(
              stats.visits(idx) > _,
              x => s"exceeded `runner.optimizer.maxVisitsPerToken`=$x",
            )

          val actualSplits = getActiveSplits(curr)(activeSplitsFilter)
          val numSplits = activeSplitsCount

          var optimalFound = false
          val handleOptimalTokens = optimizer.acceptOptimalAtHints &&
            depth < optimizer.maxDepth && numSplits > 1

          // returns whether an optimal state was found (stops sibling splits);
          // a local `var` return avoids lifting a captured var to a per-state Ref
          def processNextState(implicit nextState: State): Boolean = {
            val split = nextState.split
            val cost = split.costWithPenalty
            var found = false
            if (cost <= maxCost) {
              val opt = split.optimalAt
              val stateToQueue =
                if ((opt ne null) && handleOptimalTokens) {
                  val costToCheck =
                    if (opt.ignorePenalty) split.costWithoutPenalty else cost
                  if (costToCheck > 0) killOnFail(opt)
                  else processOptimalToken(opt) match {
                    case Left(x) => x
                    case Right(x) => found = true; x
                  }
                } else nextState
              if (null ne stateToQueue) {
                stats.updateBest(nextState, stateToQueue)
                Q.enqueue(stateToQueue)
              }
            } else preFork = false
            found
          }

          var sidx = 0
          while (sidx < numSplits) {
            val split = actualSplits(sidx)
            sidx += 1
            if (optimalFound) stats.sendEvent(split)
            else optimalFound = processNextState(getNext(curr, split))
          }
        }
      }
    }

    def endToken = {
      val okDeepest = deepestState.appliedPenalty > start.prev.appliedPenalty
      tokens(if (okDeepest) deepestState.depth else stop)
    }
    if (preFork || willKillOnFail(isKillOnFail, endToken)(start)) deadEnd
    else Left(preForkState)
  }

  private def getNext(state: State, split: Split)(implicit
      style: ScalafmtConfig,
  ): State = {
    stats.sendEvent(split)
    state.next(split)
  }

  private def willKillOnFail(kill: Boolean, end: => FT)(implicit
      nextState: State,
  ): Boolean = kill || nextState.hasSlbUntil(end)

  private def killOnFail(end: => FT)(opt: OptimalToken)(implicit
      nextState: State,
  ): State = if (willKillOnFail(opt.killOnFail, end)) null else nextState

  private def killOnFail(opt: OptimalToken)(implicit nextState: State): State =
    killOnFail(opt.token)(opt)

  private def processOptimalToken(opt: OptimalToken)(implicit
      nextState: State,
      queue: StateQueue,
      style: ScalafmtConfig,
  ): Either[State, State] = {
    val optIdx = opt.token.idx
    val nextNextState =
      if (optIdx <= nextState.depth) nextState
      else if (tokens.width(nextState.depth, optIdx) > 3 * style.maxColumn)
        return leftState(killOnFail(opt))
      else {
        val res = shortestPath(
          nextState,
          optIdx,
          queue.nested + 1,
          isOpt = true,
          isKillOnFail = opt.killOnFail,
        )
        res match {
          case Right(x) => x
          case x => return x
        }
      }
    def checkPenalty(state: State, orElse: => Either[State, State]) =
      if (state.appliedPenalty == nextNextState.appliedPenalty) orElse
      else Left(nextNextState)
    def kof = killOnFail {
      val useNextNext = (null ne nextNextState) &&
        nextNextState.appliedPenalty > nextState.prev.appliedPenalty
      if (useNextNext) tokens(nextNextState.depth) else opt.token
    }(opt)
    traverseSameLine(nextNextState) match {
      case x @ Left(s) => if (s eq null) leftState(kof) else checkPenalty(s, x)
      case x @ Right(s) => checkPenalty(s, if (opt.recurseOnly) Left(s) else x)
    }
  }

  // returns the splits array; the valid-count is written to `activeSplitsCount`
  private def getActiveSplits(
      state: State,
  )(pred: Split => Boolean)(implicit Q: StateQueue): Array[Split] = {
    stats.trackState(state)
    val idx = state.depth
    val ft = tokens(idx)
    // with no policies, `execute` is the identity, so skip the Decision alloc
    // copy into an array we own (`routes(idx)` is shared), then filter/map/sort
    // in place; `pred` may have side effects, so call it exactly once per split
    val arr = {
      val splits = routes(idx)
      if (state.policy.numPolicies == 0) splits
      else state.policy.execute(ft, splits)
    }.toArray
    var widx = 0
    var ridx = 0
    while (ridx < arr.length) {
      val s = arr(ridx)
      if (s.isActive && pred(s)) { arr(widx) = s; widx += 1 }
      ridx += 1
    }
    if (widx != 0 && (ft.meta.formatOff || !ft.inside(range))) {
      val isNL = ft.hasBreak
      val mod = Provided(ft)
      var i = 0
      while (i < widx) {
        val x = arr(i)
        val penalty = if (x.isNL == isNL) 0 else Constants.ShouldBeNewline
        arr(i) = x.withMod(mod).withPenalty(penalty)
        i += 1
      }
    }
    if (widx > 1) java.util.Arrays
      .sort(arr, 0, widx, BestFirstSearch.splitByCost)
    activeSplitsCount = widx
    arr
  }

  private def stateIsOptimal(
      state: State,
      splits: Array[Split],
      numSplits: Int,
  ): Boolean = {
    var i = 0
    while (i < numSplits) {
      val s = splits(i)
      if (s.isNL && s.penalty < Constants.ShouldBeNewline) return true
      i += 1
    }
    tokens.isRightCommentWithBreak(tokens(state.depth))
  }

  /** Follow states having single active non-newline split
    */
  @tailrec
  private def traverseSameLine(
      state: State,
  )(implicit queue: StateQueue): Either[State, State] =
    if (state.depth >= tokens.length) Right(state)
    else {
      val ss = getActiveSplits(state)(_ => true)
      val numSplits = activeSplitsCount
      if (numSplits == 0) deadEnd // dead end if empty
      else if (numSplits == 1) {
        val split = ss(0)
        if (split.isNL) Right(state)
        else {
          implicit val style: ScalafmtConfig = styleMap.at(tokens(state.depth))
          traverseSameLine(getNext(state, split))
        }
      } else if (stateIsOptimal(state, ss, numSplits)) Right(state)
      else Left(state)
    }

  def getBestPath: SearchResult = {
    if (stats.eventCallback ne null) stats
      .eventCallback(FormatEvent.Routes(routes))
    def run = shortestPath(State.start, Int.MaxValue)
    val state = run.getOrElse(stats.retry.fold(null: State) { x =>
      stats = x; run.getOrElse(null)
    })
    if (null != state) {
      stats.complete(state)
      SearchResult(state, reachedEOF = true)
    } else {
      val deepestYet = stats.deepestYet
      val nextSplits = routes(deepestYet.depth)
      val tok = tokens(deepestYet.depth)
      val splitsAfterPolicy = deepestYet.policy.execute(tok, nextSplits)
      def printSeq(vals: Iterable[_]): String = vals.mkString("\n  ", "\n  ", "")
      val msg =
        s"""|UNABLE TO FORMAT,
            |tok #${deepestYet.depth}/${tokens.length}: $tok
            |policies:${printSeq(deepestYet.policy.toIterable)}
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

  // stable in-place sort key for getActiveSplits (matches `sortBy(costWithPenalty)`)
  private val splitByCost: java.util.Comparator[Split] =
    (a: Split, b: Split) => Integer.compare(a.costWithPenalty, b.costWithPenalty)

  def apply(
      range: Set[Range],
  )(implicit formatOps: FormatOps, formatWriter: FormatWriter): SearchResult =
    new BestFirstSearch(range).getBestPath

  // dense table keyed by token idx: 0 = absent, 1 = Some(false), 2 = Some(true)
  // (an array, not a boxed-Int Map, since it's read on every search state)
  private def getNoOptZones(
      tokens: FormatTokens,
  )(implicit styleMap: StyleMap): Array[Byte] = {
    val result = new Array[Byte](tokens.length)
    var expire: FT = null
    @inline
    def addRange(ft: FT): Unit = expire = tokens.matchingLeft(ft)
    @inline
    def addBlock(idx: Int): Unit = if (result(idx) == 0) result(idx) = 1
    tokens.foreach {
      case ft if expire ne null =>
        if (ft eq expire) expire = null else result(ft.idx) = 2
      case ft @ FT(t: T.LeftParen, _, m) if (m.leftOwner match {
            case lo: Term.ArgClause => !lo.parent.is[Term.ApplyInfix] &&
              !styleMap.at(t).newlines.keep
            case _: Term.Apply => true // legacy: when enclosed in parens
            case _ => false
          }) => addRange(ft)
      case ft @ FT(t: T.LeftBrace, _, m) => m.leftOwner match {
          // Type compounds can be inside defn.defs
          case lo: meta.Stat.Block if lo.parent.is[Type.Refine] => addRange(ft)
          case _: Type.Refine => addRange(ft)
          case lo: Term.PartialFunction
              if lo.cases.lengthCompare(1) == 0 &&
                styleMap.at(t).newlines.fold => addBlock(m.idx)
          case _ =>
        }
      case _ =>
    }
    result
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

    def enqueue(state: State): Unit = generation.enqueue(state)
    def length: Int = generation.length
    @tailrec
    final def dequeue(): State =
      if (generation.isEmpty) generations match {
        case head :: tail =>
          generation = head
          generations = tail
          dequeue()
        case _ => null
      }
      else generation.dequeue()
  }

  class StateStats private (
      tokens: FormatTokens,
      runner: RunnerSettings,
      pruneSlowStates: ScalafmtOptimizer.PruneSlowStates,
  ) {
    var explored = 0
    var deepestYet = State.start
    // keyed by dense state depth; array (null = absent) avoids the per-state
    // Int-key box + Some that mutable.Map.get allocated in shouldEnterState
    val best = new Array[State](tokens.length)
    val visits = new Array[Int](tokens.length)
    private val maxVisits = runner.getMaxStateVisits

    def this(tokens: FormatTokens, runner: RunnerSettings) =
      this(tokens, runner, runner.optimizer.pruneSlowStates)

    /** Returns true if it's OK to skip over state.
      */
    def shouldEnterState(state: State): Boolean = state.policy.noDequeue ||
      (pruneSlowStates eq ScalafmtOptimizer.PruneSlowStates.No) || {
        // TODO(olafur) document why/how this optimization works.
        val b = best(state.depth)
        (b eq null) || state.possiblyBetter(b)
      }

    val eventCallback = runner.eventCallback

    def trackState(state: State)(implicit Q: StateQueue): Unit = {
      val idx = state.depth
      if (idx > deepestYet.depth) deepestYet = state
      if (eventCallback ne null) eventCallback(FormatEvent.VisitToken(tokens(idx)))
      visits(idx) += 1
      explored += 1
      if (eventCallback ne null)
        eventCallback(FormatEvent.Explored(explored, Q.nested, Q.length))
    }

    private def updateBestImpl(state: State): Boolean = state.split.isNL &&
      !state.terminal() && {
        val d = state.prev.depth
        val b = best(d)
        if (b eq null) { best(d) = state; true } else b eq state
      }

    def updateBest(state: State, furtherState: State)(implicit
        Q: StateQueue,
    ): Boolean = (pruneSlowStates ne ScalafmtOptimizer.PruneSlowStates.No) &&
      Q.nested == 0 &&
      ((state eq furtherState) || updateBestImpl(furtherState)) &&
      updateBestImpl(state)

    def checkExplored(ft: FT)(implicit formatWriter: FormatWriter): Unit =
      explode(ft, maxVisits)(
        explored > _,
        x => s"exceeded `runner.maxStateVisits`=$x",
      )

    def explode[A](ft: FT, value: A)(cond: A => Boolean, msg: A => String)(
        implicit formatWriter: FormatWriter,
    ): Unit = if (cond(value)) {
      complete(deepestYet)
      throw new Error.SearchStateExploded(deepestYet, ft, msg(value))
    }

    def complete(state: State): Unit = runner
      .completeCallback(FormatEvent.CompleteFormat(explored, state, visits, best))

    def sendEvent(split: Split): Unit =
      if (eventCallback ne null) eventCallback(FormatEvent.Enqueue(split))

    def retry: Option[StateStats] = {
      val ok = best.exists(_ ne null) &&
        (pruneSlowStates eq ScalafmtOptimizer.PruneSlowStates.Yes)
      if (ok)
        Some(new StateStats(tokens, runner, ScalafmtOptimizer.PruneSlowStates.No))
      else None
    }
  }

}
