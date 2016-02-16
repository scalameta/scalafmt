package org.scalafmt.internal

import org.scalafmt.Error
import org.scalafmt.ScalaStyle

import scala.collection.mutable
import scala.meta.Case
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
class GraphSearch(style: ScalaStyle, tree: Tree, range: Range) extends ScalaFmtLogger {

  import GraphSearch._

  val toks = FormatToken.formatTokens(tree.tokens)
  val owners = getOwners(tree)
  val statementStarts = getStatementStarts(tree)
  val memo = mutable.Map.empty[(Int, Tree), State]
  val best = mutable.Map.empty[Token, State]
  val parens = matchingParens(tree.tokens)
  // Keeps track of optimalAt optimization.
  // TODO(olafur) inefficient to key by Vector[Split]? The split prefix is what matters.
  val optimal = mutable.Map.empty[(Int, FormatToken), Split]
  val router = new Router(style, tree, toks, parens, statementStarts, owners)
  var explored = 0
  var deepestYet = State.start

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
          split.withIndent(Num(2), parens(formatToken.left), Right)
        else split
      result
    }
    val Q = new mutable.PriorityQueue[State]()
    var result = start
    Q += start
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      if (curr.splits.length > deepestYet.splits.length) {
        deepestYet = curr
      }
      explored += 1
      if (explored % 1000 == 0 && style.debug) {
        logger.debug(s"Explored $explored")
      }
      val i = curr.splits.length
      if (explored > style.maxStateVisits || i == toks.length ||
        !childOf(toks(i).right, owner, owners)) {
        result = curr
        Q.dequeueAll
      }
      else if (!pruneOK(curr)) {
        val splitToken = toks(i)
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
        val splits: List[Split] =
          if (splitToken.insideRange(range))
            router.Route(splitToken)
          else List(provided(splitToken))
        val actualSplit = curr.policy(Decision(splitToken, splits)).split
        actualSplit.withFilter(!_.ignoreIf).foreach { split =>
          val nextState = curr.next(style, split, splitToken)
          if (split.modification == Newline)
            best += splitToken.left -> nextState
          // TODO(olafur) this is a questionable optimization, it introduces

          // a lot of complexity to the search and I'm not still convinced of its
          // usefulness if we design the graph better.
          if (splitToken.left != owner.tokens.head &&
            startsUnwrappedLine(
              splitToken.left, statementStarts, owners(splitToken.left))) {
            val nextNextState =
              shortestPathMemo(owners(splitToken.left), nextState, curr)
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
  def startsUnwrappedLine(token: Token, starts: Map[Token, Tree],
                          owner: Tree): Boolean = {
    if (starts.contains(token))
      true
    else if (!owner.tokens.headOption.contains(token))
      false
    else
      owner match {
        case _: Defn | _: Case | _: Pkg => true
        case _ => false
      }
  }

  def getStatementStarts(tree: Tree): Map[Token, Tree] = {
    val ret =
      new mutable.MapBuilder[Token, Tree, Map[Token, Tree]](Map[Token, Tree]())

    def addAll(trees: Seq[Tree]): Unit = {
      trees.foreach { t =>
        ret += t.tokens.head -> t
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
      ret += firstNonAnnotation -> tree
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
  def matchingParens(tokens: Tokens): Map[Token, Token] = {
    val ret = new mutable.MapBuilder[Token, Token, Map[Token, Token]](
      Map.empty[Token, Token])
    var stack = List.empty[Token]
    tokens.foreach {
      case open@(_: `{` | _: `[` | _: `(`) => stack = open :: stack
      case close@(_: `}` | _: `]` | _: `)`) =>
        val open = stack.head
        ret += open -> close
        ret += close -> open
        stack = stack.tail
      case _ =>
    }
    val result = ret.result()
    assertValidParens(result)
    result
  }

  def assertValidParens(parens: Map[Token, Token]): Unit = {
    parens.foreach {
      case (_: `{`, _: `}`) =>
      case (_: `}`, _: `{`) =>
      case (_: `[`, _: `]`) =>
      case (_: `]`, _: `[`) =>
      case (_: `(`, _: `)`) =>
      case (_: `)`, _: `(`) =>
      case (open, close) =>
        throw new IllegalArgumentException(
          s"Mismatching parens ($open, $close)")
    }
  }

  /**
    * Creates lookup table from token to its closest scala.meta tree.
    */
  def getOwners(tree: Tree): Map[Token, Tree] = {
    val result = mutable.Map.empty[Token, Tree]
    def loop(x: Tree): Unit = {
      x.tokens.foreach { tok =>
        result += tok -> x
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

  def shouldFormat(range: Range, formatToken: FormatToken): Boolean = {
    range.contains(formatToken.left.position.start.line)
  }

}
