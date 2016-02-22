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

  val toks: Array[FormatToken] = FormatToken.formatTokens(tree.tokens)
  val owners = getOwners(tree)
  val statementStarts = getStatementStarts(tree)
  val best = mutable.Map.empty[Token, State]
  val matchingParentheses = getMatchingParentheses(tree.tokens)
  val router = new Router(style, tree, toks,
    matchingParentheses, statementStarts, owners)
  var explored = 0
  var deepestYet = State.start
  var statementCount = 0

  /**
    *
    * Returns true if it's OK to skip over state.
    */
  def pruneOK(curr: State): Boolean = {
    val splitToken = toks(curr.splits.length)
    val hasBest = best.get(splitToken.left).exists(_.alwaysBetter(curr))
    hasBest
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
    * Runs best first search to find lowest penalty split.
    */
  def shortestPath(owner: Tree, start: State): State = {
    if (style.debug) {
      Debug.visit(owner)
    }
    logger.trace(
      s"""${ start.indentation } ${start.splits.takeRight(3)}
         |${log(owner)}
         |FORMAT:
         |${ State. reconstructPath(toks, start.splits, style) }""".stripMargin)

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
      if (i == toks.length ||
        !childOf(owners(hash(toks(i).right)), owner)) {
        result = curr
        logger.trace(
          s"""
             |${log(owner, tokensOnly = true)}
             |${toks.length}
             |${curr.splits.length}
             |Q.size: ${Q.size}
             |${curr.splits.length}
           """.stripMargin)
        Q.dequeueAll
        Unit
      }
      else if (!pruneOK(curr)) {
        val splitToken = toks(i)
        if (curr.splits.length > deepestYet.splits.length) {
          deepestYet = curr
        }
        if (statementStarts.contains(hash(splitToken.left)) &&
          curr.splits.last.modification.isNewline ) {
          Q.dequeueAll
        }

        if (style.debug) {
          Debug.visit(splitToken)
        }
        if (Q.nonEmpty) {
          logger.trace(
            s"""
               |visit=$splitToken
               |lastSplit=${curr.splits.last}
               |cost=${curr.cost}
               |Q.size=${Q.size}
               |""".stripMargin)
        }
        val splits: Seq[Split] =
          if (splitToken.inside(range))
            router.getSplitsMemo(splitToken)
          else List(provided(splitToken))

        // TODO(olafur) global policies? For example inline comments must get newline.
        val actualSplit = curr.policy.execute(Decision(splitToken, splits)).splits
        if (splits.length != actualSplit.length) {
          val activePolicies = curr.policy.policies.filterNot(_.expire < splitToken.left.end)
          logger.trace(s"${activePolicies.mkString(", ")} killed $splits")
        }
        actualSplit.withFilter(!_.ignoreIf).foreach { split =>
          val nextState = curr.next(style, split, splitToken)
          // TODO(olafur) convince myself this is safe.
          if (split.modification.isNewline) {
            best += splitToken.left -> nextState
          }
          Q.enqueue(nextState)
        }
        Unit
      }
    }
    result
  }

  def formatTree(): String = {
    logger.trace(log(tree, tokensOnly = true))
    val state = shortestPath(tree, State.start)
    if (state.splits.length != toks.length) {
      val msg =
        s"""UNABLE TO FORMAT,
            |state.length=${state.splits.length}
            |toks.length=${toks.length}
            |Output:
            |${mkString(state.splits)}
            |""".stripMargin
      throw Error.CantFormatFile(msg)
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

}

object BestFirstSearch extends ScalaFmtLogger {
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
        case b: Term.Block => addAll(b.stats)
        case t: Defn.Class => addDefn[`class `](t.mods, t)
        case t: Defn.Def => addDefn[`def`](t.mods, t)
        case t: Defn.Object => addDefn[`object`](t.mods, t)
        case t: Defn.Trait => addDefn[`trait`](t.mods, t)
        case t: Defn.Type => addDefn[`type`](t.mods, t)
        case t: Defn.Val => addDefn[`val`](t.mods, t)
        case t: Defn.Var => addDefn[`var`](t.mods, t)
        case t: Pkg => addAll(t.stats)
        case t: Term.For => addAll(t.enums)
        case t: Term.ForYield => addAll(t.enums)
        case t: Term.Match => addAll(t.cases)
        case t: Term.PartialFunction => addAll(t.cases)
        case t: Type.Compound => addAll(t.refinement)
        case t: scala.meta.internal.ast.Source => addAll(t.stats)
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
    val result = mutable.Map.empty[TokenHash, Tree]
    def loop(x: Tree): Unit = {
      x.tokens.foreach { tok =>
        result += hash(tok) -> x
      }
      x.children.foreach(loop)
    }
    loop(tree)
    result.toMap
  }

}
