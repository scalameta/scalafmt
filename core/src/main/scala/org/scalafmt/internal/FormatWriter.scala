package org.scalafmt.internal

import scala.meta.Term
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

import org.scalafmt.ScalafmtStyle
import org.scalafmt.internal.FormatWriter.FormatLocation

/**
  * Produces formatted output from sequence of splits.
  */
class FormatWriter(formatOps: FormatOps) {
  import formatOps._
  import org.scalafmt.util.TreeOps._

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
    location.split.modification == Space && {
      val token = location.formatToken.right
      val code = token match {
        case c: Comment if isInlineComment(c) => "//"
        case t => t.code
      }
      style.alignMap.get(code).map { ownerRegexp =>
        val owner = owners(token) match {
          case name: Term.Name if name.parent.isDefined => name.parent.get
          case x => x
        }
        ownerRegexp.findFirstIn(owner.getClass.getName).isDefined
      }
    }.getOrElse(false)
  }

  def key(token: Token): Int =
    (token.getClass.getName, owners(token).getClass.getName).hashCode()

  private def columnsMatch(a: Array[FormatLocation],
                           b: Array[FormatLocation],
                           endOfLine: FormatToken): Int = {
    a
      .zip(b)
      .takeWhile {
        case (column1, column2) =>
          val rightOwner = column2.formatToken match {
            // Corner case when line ends with comment
            case FormatToken(x, c: Comment, _) if isInlineComment(c) =>
              owners(x)
            case FormatToken(_, r, _) => owners(r)
          }
          key(column1.formatToken.right) == key(column2.formatToken.right) &&
          !parents(owners(endOfLine.right)).contains(rightOwner)
      }
      .length
  }

  def alignmentTokens(locations: Array[FormatLocation],
                      style: ScalafmtStyle): Map[FormatToken, Int] = {
    if (style.alignTokens.isEmpty) Map.empty[FormatToken, Int]
    else {
      val finalResult = Map.newBuilder[FormatToken, Int]
      var i = 0
      var maxMatches = -1
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
//        {
//          statementStarts.contains(hash(locations(i).formatToken.right))
//        }
        val candidates = columnCandidates.result()
        if (block.isEmpty) {
          if (candidates.nonEmpty) {
            block = block :+ candidates
          }
        } else {
          val newlines = locations(i).split.modification.newlines
          val matches = columnsMatch(
              block.last, candidates, locations(i).formatToken)
          maxMatches = Math.max(maxMatches, matches)
          if (matches > 0) {
            block = block :+ candidates
          }
          if (matches == 0 || newlines > 1) {
            // Build result
            var column = 0
            val columns = maxMatches
            while (column < columns) {
              val blockWithWidth = {
                block.map { line =>
                  val columnWidth =
                    if (column == 0) {
                      line(column).state.column
                    } else {
                      val previousLocation = line(column - 1)
                      val previousColumn =
                        previousLocation.state.column -
                        previousLocation.formatToken.right.code.length
                      line(column).state.column - previousColumn
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
            maxMatches = -1
          }
        }
//        logger.elem(locations(i).formatToken)
//        if (!(statementStarts.contains(rightToken) ||
//            argumentStarts.contains(rightToken))) {
//          block = Vector.empty[Array[FormatLocation]]
//        }
        i += 1
      }
      finalResult.result()
    }
  }
}

object FormatWriter {

  case class FormatLocation(formatToken: FormatToken,
                            split: Split,
                            state: State)
}
