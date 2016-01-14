package org.scalafmt

import scala.collection.mutable
import scala.meta._
import scala.meta.tokens.Token._

class ScalaFmt(style: ScalaStyle) extends ScalaFmtLogger {

  /**
    * Pretty-prints Scala code.
    */
  def format(code: String): String = {
    val source = code.parse[Source]
    val toks = formatTokens(source.tokens)
    val path = shortestPath(source, toks)
    reconstructPath(toks, path)
  }

  /**
    * Returns formatted output from FormatTokens and Splits.
    */
  private def reconstructPath(toks: Array[FormatToken],
                              splits: Vector[Split]): String = {
    val sb = new StringBuilder()
    toks.zip(splits).foreach {
      case (tok, split) =>
        sb.append(tok.left.code)
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
  private def shortestPath(source: Source,
                           splitTokens: Array[FormatToken]): Vector[Split] = {
    val owners = getOwners(source)
    val Q = new mutable.PriorityQueue[State]()
    var explored = 0
    var result = Vector.empty[Split]
    // First state.
    Q += State(0, Vector.empty[Split])
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
        val splits = splitPenalty(owners, splitToken)
        splits.foreach {
          case (split, cost) =>
            Q.enqueue(State(curr.cost + cost, curr.path :+ split))
        }
      }
    }
    result
  } ensuring(_.length == splitTokens.length,
    "Unable to reach the last token.")

  /**
    * Assigns cost of splitting between two non-whitespace tokens.
    */
  private def splitPenalty(owners: Map[Token, Tree],
                           tok: FormatToken): List[(Split, Int)] = {
    (tok.left, tok.right) match {
      case (_: BOF, _) => List(
        NoSplit -> 0
      )
      case (_, _: EOF) => List(
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
      // TODO(olafur) Ugly hack. Is there a better way?
      case (_, _) if tok.left.name.startsWith("xml") &&
        tok.right.name.startsWith("xml") => List(
        NoSplit -> 0
      )
      case _ =>
        logger.debug(
          s"""
             |60 ===========
             |${log(tok.left)}
             |${log(tok.between: _*)}
             |${log(tok.right)}
           """.stripMargin)
        ???
    }
  }

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

  /**
    * Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as
    * little memory as possible.
    */
  private def formatTokens(tokens: Tokens): Array[FormatToken] = {
    val N = tokens.length
    require(N > 1)
    var i = 1
    var left = tokens.head
    val ts = tokens.toArray
    val result = mutable.ArrayBuilder.make[FormatToken]
    val whitespace = mutable.ArrayBuilder.make[Whitespace]()
    while (i < N) {
      ts(i) match {
        case t: Whitespace =>
          whitespace += t
        case right =>
          // TODO(olafur) avoid result.toVector
          result += FormatToken(left, right, whitespace.result.toVector)
          left = right
          whitespace.clear()
      }
      i += 1
    }
    result.result
  }
}
