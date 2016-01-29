package org.scalafmt

import scala.collection.mutable
import scala.meta._
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Term.Block
import scala.meta.internal.ast.Term.Interpolate
import scala.meta.parsers.common.Parse
import scala.meta.tokens.Token

class ScalaFmt(val style: ScalaStyle) extends ScalaFmtLogger {

  /**
    * Formats a Scala compilation unit.
    *
    * For example a source file:
    *
    * $ cat MyCode.scala
    * package foo
    *
    * import a.b
    *
    * object c {
    *   def x = 2
    *   ...
    * }
    *
    */
  def formatSource(code: String): String = format[Source](code)

  /**
    * Formats a single Scala statement.
    *
    * For example a function call:
    *
    * function(function(a, b), function(c, d))
    */
  def formatStatement(code: String): String = format[Stat](code)

  /**
    * Formats any kind of [[scala.meta.Tree]].
    *
    * Most likely, [[formatSource()]] or [[formatStatement()]] is what you need.
    *
    * @param code The source code to format.
    * @param ev The implicit evidence that the source code can be parsed.
    * @tparam T The type of the source code, refer to [[scala.meta.parsers.Api]]
    *           for available types.
    * @return The source code formatted.
    */
  def format[T <: Tree](code: String)(implicit ev: Parse[T]): String = {
    try {
      val source = code.parse[T]
      formatTree(source)
    } catch {
      // Skip invalid code.
      case e: ParseException =>
        logger.warn("Unable to parse code", e)
        code
    }
  }

  private def formatTree(tree: Tree): String = {
    val toks = FormatToken.formatTokens(tree.tokens)
    val owners = getOwners(tree)
    val statementStarts = getStatementStarts(tree)
    val memo = mutable.Map.empty[(Int, Tree), State]
    var explored = 0
    var deepestYet = State.start
    val best = mutable.Map.empty[Token, State]
    val formatter = new Formatter(style, tree, toks, statementStarts, owners)

    /**
      * Returns true if it's OK to skip over state.
      */
    def pruneOK(state: State): Boolean = {
      val splitToken = toks(state.splits.length)
      best.get(splitToken.left).exists(_.alwaysBetter(state))
    }

    /**
      * Same as shortest path except caches results.
      */
    def shortestPathMemo(owner: Tree, start: State): State = {
      val key = start.indentation -> owner
      memo.getOrElseUpdate(key, shortestPath(owner, start))
    }

    /**
      * Runs Dijstra's shortest path algorithm to find lowest penalty split.
      */
    def shortestPath(owner: Tree, start: State): State = {
      Debug.visit(owner)
      logger.trace(
        s"""${start.indentation} ${start.splits.takeRight(3)}
           |${log(owner)}
           |FORMAT:
           |${State.reconstructPath(toks, start.splits, style)}""".stripMargin)
      val Q = new mutable.PriorityQueue[State]()
      var result = start
      Q += start
      while (Q.nonEmpty) {
        val curr = Q.dequeue()
        if (curr.splits.length > deepestYet.splits.length)
          deepestYet = curr
        explored += 1
        if (explored % 100000 == 0)
          println(explored)
        val i = curr.splits.length
        if (i == toks.length ||
          !childOf(toks(i).right, owner, owners)) {
          result = curr
          Q.dequeueAll
        }
        else if (!pruneOK(curr)) {
          val splitToken = toks(i)
          Debug.visit(splitToken)
          val splits = formatter.GetSplits(splitToken)
          val actualSplit = curr.policy(Decision(splitToken, splits)).split
          actualSplit.foreach { split =>
            val nextState = curr.next(style, split, splitToken)
            if (split.modification == Newline)
              best += splitToken.left -> nextState
            if (splitToken.left != owner.tokens.head &&
              startsUnwrappedLine(splitToken.left, statementStarts,
                owners(splitToken.left))) {
              val nextNextState = shortestPathMemo(owners(splitToken.left), nextState)
              Q.enqueue(nextNextState)
            } else {
              Q.enqueue(nextState)
            }
          }
        }
      }
      result
    }

    var state = shortestPathMemo(tree, State.start)
    if (state.splits.length != toks.length) {
      state = deepestYet
      logger.warn("UNABLE TO FORMAT")
    }
    Debug.explored += explored
    Debug.state = state
    Debug.tokens = toks
    mkString(State.reconstructPath(toks, state.splits, style))
  }

  def mkString(output: Seq[(FormatToken, String)]): String = {
    val sb = new StringBuilder()
    output.foreach {
      case (tok, whitespace) =>
        sb.append(tok.left.code)
        sb.append(whitespace)
    }
    sb.toString()
  }

  def startsUnwrappedLine(token: Token,
                          starts: Set[Token],
                          owner: Tree): Boolean = {
    if (starts.contains(token)) true
    else if (!owner.tokens.headOption.contains(token)) false
    else owner match {
      case _: Defn | _: Case | _: Pkg => true
      case _ => false
    }
  }

  private def getStatementStarts(tree: Tree): Set[Token] = {
    val ret = new mutable.SetBuilder[Token, Set[Token]](Set[Token]())
    def addAll(trees: Seq[Tree]): Unit = {
      trees.foreach { t =>
        ret += t.tokens.head
      }
    }
    def loop(x: Tree): Unit = {
      x match {
        case b: Block =>
          addAll(b.stats)
        case _ => // Nothing
      }
      x.children.foreach(loop)
    }
    loop(tree)
    ret.result()
  }
  /**
    * Creates lookup table from token to its closest scala.meta tree.
    */
  private def getOwners(tree: Tree): Map[Token, Tree] = {
    val result = mutable.Map.empty[Token, Tree]
    def loop(x: Tree): Unit = {
      x.tokens
        .foreach { tok =>
          result += tok -> x
        }
      x match {
        case _: Interpolate =>
          // Nothing
        case _ =>
          x.children.foreach(loop)

      }
    }
    loop(tree)
    result.toMap
  }

}
