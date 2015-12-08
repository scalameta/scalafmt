package org.scalafmt

import scala.collection.mutable
import scala.meta._
import scala.meta.tokens.Token._

trait Split

case object NoSplit extends Split

case object Space extends Split

case object Newline extends Split

/**
  * A state represents one potential solution to reach token at index,
  * @param cost The penalty for using path
  * @param index The index of the current token.
  * @param path The splits/decicions made to reach here.
  */
case class State(cost: Int,
                 index: Int,
                 path: List[Split]) extends Ordered[State] {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: State): Int =
    (-this.cost, this.index) compare(-that.cost, that.index)
}

class ScalaFmt(style: ScalaStyle) extends ScalaFmtLogger {

  /**
    * Pretty-prints Scala code.
    */
  def format(code: String): String = {
    val source = code.parse[Source]
    val realTokens = source.tokens.filter(!_.isInstanceOf[Whitespace])
    val path = shortestPath(source, realTokens)
    val sb = new StringBuilder()
    realTokens.zip(path).foreach {
      case (tok, split) =>
        sb.append(tok.code)
        split match {
          case Space =>
            sb.append(" ")
          case Newline =>
            sb.append("\n")
          case NoSplit =>
        }
    }
    sb.toString()
  }

  /**
    * Runs Dijstra's shortest path algorithm to find lowest penalty split.
    */
  def shortestPath(source: Source, realTokens: Tokens): List[Split] = {
    val owners = getOwners(source)
    val Q = new mutable.PriorityQueue[State]()
    var explored = 0
    // First state.
    Q += State(0, 0, Nil)
    while (Q.nonEmpty) {
      val curr = Q.dequeue()
      explored += 1
      if (explored % 100000 == 0)
        println(explored)
      val tokens = realTokens
        .drop(curr.index)
        .dropWhile(_.isInstanceOf[Whitespace])
      val left = tokens.head
      if (left.isInstanceOf[EOF])
        return curr.path.reverse
      val right = tokens.tail
        .find(!_.isInstanceOf[Whitespace])
        .getOrElse(tokens.last)
      val between = tokens.drop(1).takeWhile(_.isInstanceOf[Whitespace])
      val splits = splitPenalty(owners, left, between, right)
      splits.foreach {
        case (split, cost) =>
          Q.enqueue(State(curr.cost + cost, curr.index + 1, split :: curr.path))
      }
    }
    // Could not find path to final token.
    ???
  }

  /**
    * Assigns cost of splitting between two non-whitespace tokens.
    */
  def splitPenalty(owners: Map[Token, Tree],
                   left: Token,
                   between: Tokens,
                   right: Token): List[(Split, Int)] = {
    (left, right) match {
      case (_: BOF, _) => List(
        NoSplit -> 0
      )
      case (_, _: EOF) => List(
        NoSplit -> 0
      )
      case (_, _) if left.name.startsWith("xml") &&
                       right.name.startsWith("xml") => List(
        NoSplit -> 0
      )
      case (_, _: `,`) => List(
        NoSplit -> 0
      )
      case (_: `,`, _) => List(
        Space -> 0,
        Newline -> 1
      )
      case (_: `{`, _) => List(
        Space -> 0,
        Newline -> 0
      )
      case (_, _: `{`) => List(
        Space -> 0
      )
      case (_, _: `}`) => List(
        Space -> 0,
        Newline -> 1
      )
      case (_, _: `:`) => List(
        NoSplit -> 0
      )
      case (_, _: `=`) => List(
        Space -> 0
      )
      case (_: `:` | _: `=`, _) => List(
        Space -> 0
      )
      case (_, _: `@`) => List(
        Newline -> 0
      )
      case (_: `@`, _) => List(
        NoSplit -> 0
      )
      case (_: Ident, _: `.` | _: `#`) => List(
        NoSplit -> 0
      )
      case (_: `.` | _: `#`, _: Ident) => List(
        NoSplit -> 0
      )
      case (_: Ident | _: Literal, _: Ident | _: Literal) => List(
        Space -> 0
      )
      case (_, _: `)` | _: `]`) => List(
        NoSplit -> 0
      )
      case (_, _: `(` | _: `[`) => List(
        NoSplit -> 0
      )
      case (_: `(` | _: `[`, _) => List(
        NoSplit -> 0,
        Newline -> 1
      )
      case (_, _: `val`) => List(
        Space -> 0,
        Newline -> 1
      )
      case (_: Keyword | _: Modifier, _) => List(
        Space -> 1,
        Newline -> 2
      )
      case (_, _: Keyword) => List(
        Space -> 0,
        Newline -> 1
      )
      case (_, c: Comment) => List(
        Space -> 0
      )
      case (c: Comment, _) =>
        if (c.code.startsWith("//")) List(Newline -> 0)
        else List(Space -> 0, Newline -> 1)
      case (_, _: Delim) => List(
        Space -> 0
      )
      case (_: Delim, _) => List(
        Space -> 0
      )
      case _ =>
        logger.debug(s"60 ===========\n${log(left)}\n${log(between)}\n${log(right)}")
        ???
    }
  }

  /**
    * Creates lookup table from token to its closest scala.meta contains tree.
    */
  def getOwners(source: Source): Map[Token, Tree] = {
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
