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
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens
import scala.reflect.ClassTag
import scala.reflect.classTag

/**
  * Implements Dijkstra's shortest path search to find optimal formatting.
  */
class GraphSearch(style: ScalaStyle, tree: Tree, range: Int => Boolean)
  extends ScalaFmtLogger {

  import GraphSearch._

  val toks = FormatToken.formatTokens(tree.tokens)
  val owners = getOwners(tree)
  val statementStarts = getStatementStarts(tree)
  val memo = mutable.Map.empty[(Int, Tree), State]
  val best = mutable.Map.empty[Token, State]
  val matchingParentheses = getMatchingParentheses(tree.tokens)
  // TODO(olafur) better name.
  val optimal = mutable.Map.empty[(Int, FormatToken), Split]
  val router = new Router(style, tree, toks,
    matchingParentheses, statementStarts, owners)
  var explored = 0
  var deepestYet = State.start
  var statementCount = 0
  /**
    *
    * Returns true if it's OK to skip over state.
    */
  def pruneOK(state: State): Boolean = {
    val splitToken = toks(state.splits.length)
    // TODO(olafur) super inefficient
    val hasOptimal = state.splits.zipWithIndex.forall {
      case (split, i) =>
        val tok = toks(i)
        val result = optimal.get(state.states(i).column -> tok)
          .forall(_.sameLine(split))
        if (!result) {
          logger.trace(
            s"""
               |${header(s"$split eliminated $state at ${toks(i)}, $tok")}
               |${mkString(state.splits)}
               |${optimal.toVector.mkString("\n")}""".stripMargin)
        }
        result
    }
    val hasBest = best.get(splitToken.left).exists(_.alwaysBetter(state))
    !hasOptimal || hasBest
  }

  def updateOptimal(tok: FormatToken, curr: State): Unit = {
    // TODO(olafur) inefficient
    curr.splits.zipWithIndex.foreach {
      case (split, i) =>
        val currTok = toks(i)
        if (split.optimalAt.contains(tok.left)) {
          logger.trace(
            s"""optimal $curr $split ${curr.cost} $tok
                |${header("output")}
                |${mkString(curr.splits)}
                """.stripMargin)
          optimal += (curr.states(i).column -> currTok) -> split
        }
    }
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

  /**
    *
    * Runs Dijstra's shortest path algorithm to find lowest penalty split.
    */
  def shortestPath(owner: Tree, start: State): State = {
    if (style.debug) {
      Debug.visit(owner)
    }
    logger.trace(
      s"""${
        start.indentation
      } ${start.splits.takeRight(3)}

         |${log(owner)}

         |FORMAT:
         |${
        State.
          reconstructPath(toks, start.splits, style)
      }""".
        stripMargin)

    val Q = new mutable.PriorityQueue[State]()
    var result = start
    Q += start
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      explored += 1
      if (explored % 1000 == 0 && style.debug) {
        logger.debug(s"Explored $explored")
      }
      val i = curr.splits.length
      if (explored > style.maxStateVisits || i == toks.length ||
        !childOf(toks(i).right, owner, owners)) {
        result = curr
        logger.trace(
          s"""Q.size: ${Q.size}
              |${Q.map(x => curr.splits.length - x.splits.length).mkString("\n")}
           """.stripMargin)
        Q.dequeueAll
      }
      else if (!pruneOK(curr)) {
        val splitToken = toks(i)
        if (curr.splits.length > deepestYet.splits.length) {
          deepestYet = curr
        }
        updateOptimal(splitToken, curr)
        if (style.debug) {
          Debug.visit(splitToken)
        }
        if (Q.nonEmpty) {
          val minCost = Q.minBy(_.cost)
          logger.trace(
            s"""
               |visit=$splitToken
               |lastSplit=${curr.splits.last}
               |cost=${curr.cost}
               |minCost=${minCost.cost}
               |Q.size=${Q.size}
               |""".stripMargin)
        }
        val splits: Seq[Split] =
          if (splitToken.inside(range))
            router.Route(splitToken)
          else List(provided(splitToken))
        val actualSplit = curr.policy.execute(Decision(splitToken, splits)).splits
        if (splits.length != actualSplit.length) {
          logger.trace(s"${curr.policy.policies.mkString(", ")} killed $splits")
        }
        actualSplit.withFilter(!_.ignoreIf).foreach { split =>
          val nextState = curr.next(style, split, splitToken)
          if (split.modification == Newline)
            best += splitToken.left -> nextState
          if (splitToken.left != owner.tokens.head &&
            statementStarts.contains(hash(splitToken.left))) {
            val nextNextState =
              shortestPathMemo(owners(hash(splitToken.left)), nextState, curr)
            Q.enqueue(nextNextState)
          }
          else {
            Q.enqueue(nextState)
          }
        }
      }
    }
    result
  }

  def formatTree(): String = {
    var state = shortestPathMemo(tree, State.start, State.start)
    if (state.splits.length != toks.length) {
      if (style.debug) {
        logger.warn("UNABLE TO FORMAT")
        state = deepestYet
      }
      else {
        throw Error.CantFormatFile
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

  /**
    * Same as shortest path except caches results.
    */
  private def shortestPathMemo(owner: Tree, start: State, prev: State): State = {
    val col = start.indentation
    val i = Math.max(0, prev.splits.length - 1)
    val key = col -> owner
    memo.getOrElseUpdate(key, {
      val result = shortestPath(owner, start)
      val output = mkString(result.splits)
      logger.trace(
        s"""${result.cost} ${result.column} ${prev.column} ${prev.indentation} ${start.column} ${toks(i)}
           |${header("splits")}
           |${start.splits}
           |${header("stripped output")}
           |${output.stripPrefix(mkString(start.splits))}
           |${header("prev.splits")}
           |${mkString(prev.splits).length}
           |${mkString(prev.splits)}
           |${reveal(mkString(prev.splits))}
           |${header("output")}
           |${mkString(result.splits)}
           |""".stripMargin)
      result
    })
  }

}

object GraphSearch {
  def getStatementStarts(tree: Tree): Map[Long, Tree] = {
    val ret =
      new mutable.MapBuilder[Long, Tree, Map[Long, Tree]](Map[Long, Tree]())

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
        case t: scala.meta.internal.ast.Source => addAll(t.stats)
        case t: Pkg => addAll(t.stats)
        case t: Term.ForYield => addAll(t.enums)
        case t: Term.For => addAll(t.enums)
        case t: Term.Match => addAll(t.cases)
        case t: Term.PartialFunction => addAll(t.cases)
        case b: Term.Block => addAll(b.stats)
        case t: Defn.Object => addDefn[`object`](t.mods, t)
        case t: Defn.Class => addDefn[`class `](t.mods, t)
        case t: Defn.Trait => addDefn[`trait`](t.mods, t)
        case t: Defn.Def => addDefn[`def`](t.mods, t)
        case t: Defn.Val => addDefn[`val`](t.mods, t)
        case t: Defn.Var => addDefn[`var`](t.mods, t)
        case t: Defn.Type => addDefn[`type`](t.mods, t)
        case t: Template if t.stats.isDefined =>
          addAll(t.stats.get)
        case _ => // Nothing
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
  def getMatchingParentheses(tokens: Tokens): Map[Long, Token] = {
    val ret = new mutable.MapBuilder[Long, Token, Map[Long, Token]](
      Map.empty[Long, Token])
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
  def getOwners(tree: Tree): Map[Long, Tree] = {
    val result = mutable.Map.empty[Long, Tree]
    def loop(x: Tree): Unit = {
      x.tokens.foreach { tok =>
        result += hash(tok) -> x
      }
      x match {
        case _: Term.Interpolate => // TODO(olafur) the mod is unintuitive
        //        case _: scala.meta.internal.ast.Mod.Override.Api =>
        // Nothing
        case _ => x.children.foreach(loop)
      }
    }
    loop(tree)
    result.toMap
  }

}
