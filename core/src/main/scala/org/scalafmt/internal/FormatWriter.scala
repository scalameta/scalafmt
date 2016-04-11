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
    FormatWriter.reconstructPath(tokens, splits, style) {
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
}

object FormatWriter {
  case class FormatLocation(
      formatToken: FormatToken, split: Split, state: State)
  import org.scalafmt.util.LoggerOps._

  def getStates(toks: Array[FormatToken],
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
    val states = getStates(toks, splits, style, debug)
    states.zipWithIndex.foreach {
      case (FormatLocation(tok, split, state), i) =>
        val whitespace = split.modification match {
          case Space => " "
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
    if (debug) logger.debug(s"Total cost: ${states.last.split.cost}")
  }
}
