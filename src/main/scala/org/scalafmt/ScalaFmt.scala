package org.scalafmt

import scala.collection.mutable
import scala.meta._
import scala.meta.tokens.Token

class ScalaFmt(style: ScalaStyle) extends ScalaFmtLogger {

  /**
    * Penalty to kill the current path.
    */
  val KILL = 10000

  /**
    * Pretty-prints Scala code.
    */
  def format(code: String): String = {
    val source = code.parse[Source]
    val toks = FormatToken.formatTokens(source.tokens)
    val path = shortestPath(source, toks)
    reconstructPath(toks, path)
  }

  /**
    * Returns formatted output from FormatTokens and Splits.
    */
  private def reconstructPath(toks: Array[FormatToken],
                              splits: Vector[Split]): String = {
    require(toks.length == splits.length)
    val sb = new StringBuilder()
    var state = State.start
    toks.zip(splits).foreach {
      case (tok, split) =>
        state = next(state, split, tok)
        //        logger.debug(s"${log(tok.left)} $split ${state.indentation}")
        sb.append(tok.left.code)
        val ws = split.modification match {
          case Space =>
            sb.append(" ")
          case Newline =>
            sb.append("\n" + " " * state.indentation)
          case _ =>
          // Nothing
        }
    }
    sb.toString()
  }

  /**
    * Calculates next State given split at tok.
    *
    * - Accumulates cost and strategies
    * - Calculates column-width overflow penalty
    */
  private def next(state: State,
                   split: Split,
                   tok: FormatToken): State = {
    // TODO(olafur) performance alert...
    val newIndents = split.indent.foldLeft(state.indents) {
      case (pushes, indent) => indent match {
        case PushStateColumn =>
          val i = state.column - state.indentation
          Push(i) +: pushes
        case p: Push =>
          p +: pushes
        case NoOp =>
          pushes
        case Pop =>
          if (pushes.nonEmpty) pushes.tail
          else throw TooManyIndentPops
      }
    }
    val newIndent = newIndents.foldLeft(0)(_ + _.num)
    // Always account for the cost of the right token.
    val newColumn = tok.right.code.length + (
      if (split.modification == Newline) newIndent
      else state.column + split.length)
    val splitWithPenalty =
      if (newColumn < style.maxColumn) split
      else split.withPenalty(KILL)
    val newPolicy =
      if (split.policy == NoPolicy) state.policy
      else (split.policy orElse IdentityPolicy) andThen state.policy
    State(state.cost + splitWithPenalty.cost,
      // TODO(olafur) expire policy, see #18.
      newPolicy,
      state.path :+ splitWithPenalty,
      newIndent,
      newIndents,
      newColumn)
  }

  /**
    * Runs Dijstra's shortest path algorithm to find lowest penalty split.
    */
  private def shortestPath(source: Source,
                           splitTokens: Array[FormatToken]): Vector[Split] = {
    val formatter = new Formatter(style, getOwners(source))
    val Q = new mutable.PriorityQueue[State]()
    var explored = 0
    var result = Vector.empty[Split]
    Q += State.start
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      if (curr.path.length == splitTokens.length) {
        result = curr.path
        Q.dequeueAll
      }
      else {
        explored += 1
        if (explored % 100000 == 0)
          println(explored)
        val splitToken = splitTokens(curr.path.length)
        val splits = formatter.GetSplits(splitToken)
        val actualSplit = curr.policy(Decision(curr, splitToken, splits)).split
        actualSplit.foreach { split =>
          val nextState = next(curr, split, splitToken)
          Q.enqueue(nextState)
        }
      }
    }
    result
  } ensuring(_.length == splitTokens.length, "Unable to reach the last token.")

  /**
    * Creates lookup table from token to its closest scala.meta tree.
    */
  private def getOwners(source: Source): Map[Token, Tree] = {
    val result = mutable.Map.empty[Token, Tree]
    def loop(x: Tree): Unit = {
      x.tokens
        .foreach { tok =>
          result += tok -> x
        }
      x.children.foreach(loop)
    }
    loop(source)
    result.toMap
  }

}
