package org.scalafmt.internal

import scala.meta.tokens.Token
import scala.meta.tokens.Token._

import org.scalafmt.ScalafmtStyle

/**
  * Produces formatted output from sequence of splits.
  */
class FormatWriter(formatOps: FormatOps) {
  import formatOps._

  def mkString(splits: Vector[Split]): String = {
    val sb = new StringBuilder()
    var lastState =
      State.start // used to calculate start of formatToken.right.
    reconstructPath(tokens, splits, style) {
      case (state, formatToken, whitespace) =>
        formatToken.left match {
          case c: Comment if c.code.startsWith("/*") =>
            sb.append(formatComment(c, state.indentation))
          case token: Interpolation.Part =>
            sb.append(formatMarginizedString(token, state.indentation))
          case literal: Literal.String => // Ignore, see below.
          case token => sb.append(token.code)
        }
        sb.append(whitespace)
        formatToken.right match {
          // state.column matches the end of formatToken.right
          case literal: Literal.String =>
            val column =
              if (state.splits.last.modification.isNewline) state.indentation
              else lastState.column + whitespace.length
            sb.append(formatMarginizedString(literal, column + 2))
          case _ => // Ignore
        }
        lastState = state
    }
    sb.toString()
  }

  private def formatComment(comment: Comment, indent: Int): String = {
    val isDocstring = comment.code.startsWith("/**")
    val spaces: String =
      if (isDocstring && style.scalaDocs) " " * (indent + 2)
      else " " * (indent + 1)
    comment.code.replaceAll("\n *\\*", s"\n$spaces\\*")
  }

  private def formatMarginizedString(token: Token, indent: Int): String = {
    if (!style.alignStripMarginStrings) token.code
    else if (token.isInstanceOf[Interpolation.Part] ||
             isMarginizedString(token)) {
      val spaces = " " * indent
      token.code.replaceAll("\n *\\|", s"\n$spaces\\|")
    } else {
      token.code
    }
  }

  case class FormatLocation(formatToken: FormatToken,
                            split: Split,
                            state: State)
  import org.scalafmt.util.LoggerOps._
  import org.scalafmt.util.TokenOps._

  def getFormatLocations(toks: Array[FormatToken],
                         splits: Vector[Split],
                         style: ScalafmtStyle,
                         debug: Boolean): Array[FormatLocation] = {
    require(toks.length >= splits.length, "splits !=")
    val statesBuilder = Array.newBuilder[FormatLocation]
    statesBuilder.sizeHint(toks.length)
    var currState = State.start
    splits.zipWithIndex.foreach {
      case (split, i) =>
        val tok = toks(i)
        currState = State.next(currState, style, split, tok)
        statesBuilder += FormatLocation(tok, split, currState)
        // TIP. Use the following line to debug origin of splits.
        if (debug && toks.length < 1000) {
          val left = cleanup(tok.left).slice(0, 15)
          logger.debug(
              f"$left%-15s $split ${currState.indentation} ${currState.column}")
        }
    }
    statesBuilder.result()
  }

  /**
    * Reconstructs path for all tokens and invokes callback for each token/split
    * combination.
    */
  def reconstructPath(toks: Array[FormatToken],
                      splits: Vector[Split],
                      style: ScalafmtStyle,
                      debug: Boolean = false)(
      callback: (State, FormatToken, String) => Unit): Unit = {
    require(toks.length >= splits.length, "splits !=")
    val locations = getFormatLocations(toks, splits, style, debug)
    val tokenAligns = alignmentTokens(locations, style).withDefaultValue(0)
    locations.zipWithIndex.foreach {
      case (FormatLocation(tok, split, state), i) =>
        val whitespace = split.modification match {
          case Space => " " * (1 + tokenAligns(tok))
          case nl: NewlineT =>
            val newline =
              if (nl.isDouble) "\n\n"
              else "\n"
            val indentation =
              if (nl.noIndent) ""
              else " " * state.indentation
            newline + indentation
          case Provided(literal) => literal
          case NoSplit => ""
        }
        callback.apply(state, tok, whitespace)
    }
    if (debug) logger.debug(s"Total cost: ${locations.last.split.cost}")
  }

  private def isCandidate(
      location: FormatLocation, style: ScalafmtStyle): Boolean = {
    location.split.modification == Space &&
    style.alignRegexp.findFirstIn(location.formatToken.right.code).isDefined
  }

  def key(token: Token): Int = (token.getClass.getName).hashCode()

  private def columnsMatch(
      a: Array[FormatLocation], b: Array[FormatLocation]): Boolean = {
    if (a.length != b.length) false
    else {
      a.zip(b).forall {
        case (left, right) =>
          key(left.formatToken.right) == key(right.formatToken.right)
      }
    }
  }

  def alignmentTokens(locations: Array[FormatLocation],
                      style: ScalafmtStyle): Map[FormatToken, Int] = {
    if (style.alignTokens.isEmpty) Map.empty[FormatToken, Int]
    else {
      val finalResult = Map.newBuilder[FormatToken, Int]
      var i = 0
      var block = Vector.empty[Array[FormatLocation]]
      while (i < locations.length) {
        val columnCandidates = Array.newBuilder[FormatLocation]
        while (i < locations.length &&
        !locations(i).split.modification.isNewline) {
          // One row
          if (isCandidate(locations(i), style)) {
            columnCandidates += locations(i)
          }
          i += 1
        }
        val candidates = columnCandidates.result()
        if (block.isEmpty) {
          if (candidates.nonEmpty) {
            block = block :+ candidates
          }
        } else {
          val newlines = locations(i).split.modification.newlines
          val isMatch = columnsMatch(block.last, candidates)
          if (isMatch) {
            block = block :+ candidates
          }
          if (!isMatch || newlines > 1) {
            // Build result
            var column = 0
            val columns = block.head.length
            while (column < columns) {
              val blockWithWidth = {
                block.map { line =>
                  val columnWidth =
                    if (column == 0) {
                      line(column).state.column
                    } else {
                      line(column).state.column - line(column - 1).state.column
                    }
                  val key =
                    columnWidth - line(column).formatToken.right.code.length
                  key -> line(column)
                }
              }
              val (maxWidth, _) = blockWithWidth.maxBy(_._1)
              blockWithWidth.foreach {
                case (width, line) =>
                  finalResult += line.formatToken -> (maxWidth - width)
              }
              column += 1
            }
            block = Vector.empty[Array[FormatLocation]]
          }
        }
        i += 1
      }
      finalResult.result()
    }
  }
}
