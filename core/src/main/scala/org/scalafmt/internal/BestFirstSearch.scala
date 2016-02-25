package org.scalafmt.internal

import org.scalafmt.Error
import org.scalafmt.ScalaStyle

import scala.collection.mutable
import scala.meta.Mod
import scala.meta.Tree
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Template
import scala.meta.internal.ast.Term
import scala.meta.internal.ast.Type
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens
import scala.reflect.ClassTag
import scala.reflect.classTag

/**
  * Implements best first search to find optimal formatting.
  */
class BestFirstSearch(style: ScalaStyle, tree: Tree, range: Set[Range])
  extends ScalaFmtLogger {

  import BestFirstSearch._

  val maxVisitStates =  // For debugging purposes only.
    if (style.debug) 100000 // Unit tests must be < 100k states
    else 10000000

  val doOptimizations = true // For debugging purposes only.

  /**
    * Eliminate solutions that are inferior to already explored solutions.
    *
    * If a solution is reaches a point at cost X then any other solution that
    * reaches the same token with a cost > X will be eliminated.
    */
  val pruneNonOptimal = true && doOptimizations

  /**
    * When entering a new statement, clear out search queue.
    */
  val dequeOnNewStatements = true && doOptimizations

  /**
    * Recursively format {} wrapped blocks inside no optimization zones.
    *
    * By starting a new search queue, we can perform aggressive optimizations
    * inside optimizations zones.
    */
  val recurseOnBlocks = true && doOptimizations

  val toks: Array[FormatToken] = FormatToken.formatTokens(tree.tokens)
  val owners = getOwners(tree)
  val statementStarts = getStatementStarts(tree)
  val matchingParentheses = getMatchingParentheses(tree.tokens)
  val noOptimizations = noOptimizationZones(tree)
  val best = mutable.Map.empty[Token, State]
  val router = new Router(style, tree, toks,
    matchingParentheses, statementStarts, owners)
  var explored = 0
  var deepestYet = State.start
  var statementCount = 0

  type StateHash = Long


  def isInsideNoOptZone(token: FormatToken): Boolean = {
    noOptimizations.contains(token.left)
  }

  /**
    * Returns true if it's OK to skip over state.
    */
  def shouldEnterState(curr: State): Boolean = {
    val splitToken = toks(curr.splits.length)
    val insideOptimizationZone =
      curr.policy.noDequeue || isInsideNoOptZone(splitToken)
    def hasBestSolution = !pruneNonOptimal || insideOptimizationZone || {
      val splitToken = toks(curr.splits.length)
      // TODO(olafur) document why/how this optimization works.
      !best.get(splitToken.left).exists(_.alwaysBetter(curr))
    }
   hasBestSolution
  }

  def getLeftLeft(curr: State): Token = {
    toks(Math.max(0, curr.splits.length - 1)).left
  }

  def shouldRecurseOnBlock(curr: State, stop: Token) = {
    val leftLeft = getLeftLeft(curr)
    val leftLeftOwner = owners(hash(leftLeft))
    val splitToken = toks(curr.splits.length)
    recurseOnBlocks &&
      isInsideNoOptZone(splitToken) &&
      leftLeft.isInstanceOf[`{`] &&
      matchingParentheses(hash(leftLeft)) != stop &&
      {
        // Block must span at least 3 lines to be worth recursing.
        val close = matchingParentheses(hash(leftLeft))
        // TODO(olafur) magic number
        close.start - leftLeft.end > style.maxColumn * 3
      } &&
      extractStatementsIfAny(leftLeftOwner).nonEmpty
  }

  def provided(formatToken: FormatToken): Split = {
    // TODO(olafur) the indentation is not correctly set.
    val split =
      Split(Provided(formatToken.between.map(_.code).mkString), 0)
    val result =
      if (formatToken.left.isInstanceOf[`{`])
        split.withIndent(Num(2),
          matchingParentheses(hash(formatToken.left)), Right)
      else split
    result
  }

  def stateColumnKey(state: State): Int = {
    state.column << 8 | state.indentation
  }

  def hasReachedEof(state: State): Boolean = {
    explored > maxVisitStates || state.splits.length == toks.length
  }

  val memo = mutable.Map.empty[(Int, StateHash), State]

  def shortestPathMemo(start: State, stop: Token, depth: Int): State = {
    memo.getOrElseUpdate((start.splits.length, stateColumnKey(start)), {
      shortestPath(start, stop, depth)
    })
  }

  /**
    * Runs best first search to find lowest penalty split.
    */
  def shortestPath(start: State, stop: Token, depth: Int = 0,
                   maxCost: Int = Integer.MAX_VALUE): State = {
    val Q = new mutable.PriorityQueue[State]()
    var result = start
    Q += start
    // TODO(olafur) this while loop is waaaaaaaaaaaaay tooo big.
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      explored += 1
      if (explored % 10000 == 0 && style.debug) {
        logger.debug(s"Explored $explored")
      }
      if (hasReachedEof(curr) ||
        toks(curr.splits.length).left == stop) {
        result = curr
        Q.dequeueAll
        Unit
      } else if (shouldEnterState(curr)) {
        val splitToken = toks(curr.splits.length)
        if (curr.splits.length > deepestYet.splits.length) {
          deepestYet = curr
          // Only dequeue if this solution is the deepest solution so far.
        }

        if (dequeOnNewStatements &&
          (depth > 0 || !isInsideNoOptZone(splitToken)) &&
          statementStarts.contains(hash(splitToken.left)) &&
          curr.splits.last.modification.isNewline ) {
          Q.dequeueAll
        }

        if (style.debug) {
          Debug.visit(splitToken)
        }
        if (shouldRecurseOnBlock(curr, stop)) {
          val nextState = shortestPathMemo(
            curr,
            matchingParentheses(hash(getLeftLeft(curr))),
            depth = depth + 1)
          Q.enqueue(nextState)
        } else {

          val splits: Seq[Split] =
            if (splitToken.inside(range))
              router.getSplitsMemo(splitToken)
            else List(provided(splitToken))



          val actualSplit = {
            curr.policy
              .execute(Decision(splitToken, splits)).splits
              .filter(!_.ignoreIf)
              .sortBy(_.cost)
          }
          var optimalNotFound = true
          actualSplit.foreach { split =>
            val nextState = curr.next(style, split, splitToken)
            // TODO(olafur) convince myself this is safe.
            if (split.modification.isNewline) {
              best.update(splitToken.left, nextState)
            }
            if (style.debug) {
              Debug.enqueued(split)
            }
            split.optimalAt match {
              case Some(token) if actualSplit.length > 1 && split.cost == 0 =>
                val nextNextState = shortestPath(nextState, token, depth, maxCost = 0)
                if (toks(nextNextState.splits.length).left == token) {
                  optimalNotFound = false
                  Q.enqueue(nextNextState)
                } else if (nextState.cost - curr.cost <= maxCost) {
                  // TODO(olafur) DRY. This solution can still be optimal.
                  Q.enqueue(nextState)
                }
              case _ if optimalNotFound && nextState.cost - curr.cost <= maxCost =>
                Q.enqueue(nextState)
              case _ => // Other split was optimal
            }
          }
        }
        Unit
      }
    }
    result
  }

  def formatTree(): String = {
    var state = shortestPath(State.start, tree.tokens.last)
    if (state.splits.length != toks.length) {
      if (style.debug) {
        state = deepestYet
      } else {
        val msg =
          s"""UNABLE TO FORMAT,
              |state.length=${state.splits.length}
              |toks.length=${toks.length}
              |deepestYet.length=${deepestYet.splits.length}
              |Output:
              |${mkString(deepestYet.splits)}
              |""".stripMargin
        throw Error.CantFormatFile(msg)
      }
    }
    if (style.debug) {
      Debug.explored += explored
      Debug.state = state
      Debug.tokens = toks
    }
    mkString(state.splits)
  }

  private def mkString(splits: Vector[Split]): String = {
    val output = State.reconstructPath(toks, splits, style)
    val sb = new StringBuilder()
    output.foreach {
      case (tok, whitespace) =>
        sb.append(tok.left.code)
        sb.append(whitespace)
    }
    sb.toString()
  }

  def noOptimizationZones(tree: Tree): Set[Token] = {
    val result = new mutable.SetBuilder[Token, Set[Token]](Set.empty[Token])
    var inside = false
    var expire = tree.tokens.head
    tree.tokens.foreach {
      case t: `(` if owners(hash(t)).isInstanceOf[Term.Apply] && !inside =>
        inside = true
        expire = matchingParentheses(hash(t))
      case x if x == expire =>
        inside = false
      case x if inside =>
        result += x
      case _ =>
    }
    result.result()
  }

}

object BestFirstSearch extends ScalaFmtLogger {

  def extractStatementsIfAny(tree: Tree): Seq[Tree] = tree match {
    case b: Term.Block => b.stats
    case t: Pkg => t.stats
    case t: Term.For => t.enums
    case t: Term.ForYield => t.enums
    case t: Term.Match => t.cases
    case t: Term.PartialFunction => t.cases
    case t: Term.TryWithCases => t.catchp
    case t: Type.Compound => t.refinement
    case t: scala.meta.internal.ast.Source => t.stats
    case t: Template if t.stats.isDefined => t.stats.get
    case _ => Seq.empty[Tree]
  }


  def getStatementStarts(tree: Tree): Map[TokenHash, Tree] = {
    val ret =
      new mutable.MapBuilder[TokenHash, Tree, Map[TokenHash, Tree]](Map[TokenHash, Tree]())

    def addAll(trees: Seq[Tree]): Unit = {
      trees.foreach { t =>
        ret += hash(t.tokens.head) -> t
      }
    }

    def addDefn[T <: Keyword : ClassTag](mods: Seq[Mod], tree: Tree): Unit = {
      // Each @annotation gets a separate line
      val annotations = mods.filter(_.isInstanceOf[Mod.Annot])
      addAll(annotations)
      val firstNonAnnotation: Token = mods.collectFirst {
        case x if !x.isInstanceOf[Mod.Annot] =>
          // Non-annotation modifier, for example `sealed`/`abstract`
          x.tokens.head
      }.getOrElse {
        // No non-annotation modifier exists, fallback to keyword like `object`
        tree.tokens.find(x => classTag[T].runtimeClass.isInstance(x)) match {
          case Some(x) => x
          case None => throw Error.CantFindDefnToken[T](tree)
        }
      }
      ret += hash(firstNonAnnotation) -> tree
    }

    def loop(x: Tree): Unit = {
      x match {
        case t: Defn.Class => addDefn[`class `](t.mods, t)
        case t: Defn.Def => addDefn[`def`](t.mods, t)
        case t: Defn.Object => addDefn[`object`](t.mods, t)
        case t: Defn.Trait => addDefn[`trait`](t.mods, t)
        case t: Defn.Type => addDefn[`type`](t.mods, t)
        case t: Defn.Val => addDefn[`val`](t.mods, t)
        case t: Defn.Var => addDefn[`var`](t.mods, t)
        case t => // Nothing
          addAll(extractStatementsIfAny(t))
      }
      x.children.foreach(loop)
    }
    loop(tree)
    ret.result()
  }

  /**
    * Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  def getMatchingParentheses(tokens: Tokens): Map[TokenHash, Token] = {
    val ret = new mutable.MapBuilder[TokenHash, Token, Map[TokenHash, Token]](
      Map.empty[TokenHash, Token])
    var stack = List.empty[Token]
    tokens.foreach {
      case open@(_: `{` | _: `[` | _: `(`) => stack = open :: stack
      case close@(_: `}` | _: `]` | _: `)`) =>
        val open = stack.head
        assertValidParens(open, close)
        ret += hash(open) -> close
        ret += hash(close) -> open
        stack = stack.tail
      case _ =>
    }
    val result = ret.result()
    result
  }

  def assertValidParens(open: Token, close: Token): Unit = {
    (open, close) match {
      case (_: `{`, _: `}`) =>
      case (_: `[`, _: `]`) =>
      case (_: `(`, _: `)`) =>
      case (o, c) =>
        throw new IllegalArgumentException(
          s"Mismatching parens ($o, $c)")
    }
  }

  /**
    * Creates lookup table from token offset to its closest scala.meta tree.
    */
  def getOwners(tree: Tree): Map[TokenHash, Tree] = {
    val result = new mutable.MapBuilder[TokenHash, Tree, Map[TokenHash, Tree]](
      Map.empty[TokenHash, Tree])
    def loop(x: Tree): Unit = {
      x.tokens.foreach { tok =>
        result += hash(tok) -> x
      }
      x.children.foreach(loop)
    }
    loop(tree)
    result.result()
  }

}
