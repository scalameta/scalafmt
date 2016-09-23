package org.scalafmt.internal

import scala.meta.Term
import scala.meta.Tree
import scala.meta.prettyprinters.Syntax
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

import java.util.regex.Pattern

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
    var lastState = State.start // used to calculate start of formatToken.right.
    reconstructPath(tokens, splits, style, debug = false) {
      case (state, formatToken, whitespace) =>
        formatToken.left match {
          case c: Comment =>
            sb.append(formatComment(c, state.indentation))
          case token @ Interpolation.Part(_) =>
            sb.append(formatMarginizedString(token, state.indentation))
          case literal @ Constant.String(_) => // Ignore, see below.
          case token =>
            val rewrittenToken =
              style.rewriteTokens.getOrElse(token.syntax, token.syntax)
            sb.append(rewrittenToken)
        }
        sb.append(whitespace)
        formatToken.right match {
          // state.column matches the end of formatToken.right
          case literal: Constant.String =>
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

  val trailingSpace = Pattern.compile(" +$", Pattern.MULTILINE)
  private def removeTrailingWhiteSpace(str: String): String = {
    trailingSpace.matcher(str).replaceAll("")
  }

  val leadingAsteriskSpace =
    Pattern.compile("\n *\\*(?!\\*)", Pattern.MULTILINE)
  private def formatComment(comment: Comment, indent: Int): String = {
    val alignedComment =
      if (comment.syntax.startsWith("/*") && style.reformatDocstrings) {
        val isDocstring = comment.syntax.startsWith("/**")
        val spaces: String =
          if (isDocstring && style.scalaDocs) " " * (indent + 2)
          else " " * (indent + 1)
        leadingAsteriskSpace
          .matcher(comment.syntax)
          .replaceAll(s"\n$spaces\\*")
      } else {
        comment.syntax
      }
    removeTrailingWhiteSpace(alignedComment)
  }

  val leadingPipeSpace = Pattern.compile("\n *\\|", Pattern.MULTILINE)
  private def formatMarginizedString(token: Token, indent: Int): String = {
    if (!style.assumeStandardLibraryStripMargin) token.syntax
    else if (token.is[Interpolation.Part] ||
             isMarginizedString(token)) {
      val firstChar: Char = token match {
        case Interpolation.Part(_) =>
          (for {
            parent <- owners(token).parent
            firstInterpolationPart <- parent.tokens.find(
              _.is[Interpolation.Part])
            char <- firstInterpolationPart.syntax.headOption
          } yield char).getOrElse(' ')
        case _ =>
          token.syntax.find(_ != '"').getOrElse(' ')
      }
      val extraIndent: Int = if (firstChar == '|') 1 else 0
      val spaces = " " * (indent + extraIndent)
      leadingPipeSpace.matcher(token.syntax).replaceAll(s"\n$spaces\\|")
    } else {
      token.syntax
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
        if (debug && tokens.length < 1000) {
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
  def reconstructPath(
      toks: Array[FormatToken],
      splits: Vector[Split],
      style: ScalafmtStyle,
      debug: Boolean)(callback: (State, FormatToken, String) => Unit): Unit = {
    require(toks.length >= splits.length, "splits !=")
    val locations = getFormatLocations(toks, splits, style, debug)
    val tokenAligns = alignmentTokens(locations, style).withDefaultValue(0)
    var lastModification = locations.head.split.modification
    locations.zipWithIndex.foreach {
      case (FormatLocation(tok, split, state), i) =>
        val previous = locations(Math.max(0, i - 1))
        val whitespace = split.modification match {
          case Space =>
            val previousAlign =
              if (lastModification == NoSplit) tokenAligns(prev(tok))
              else 0
            " " + (" " * (tokenAligns(tok) + previousAlign))
          case nl: NewlineT
              if nl.acceptNoSplit && !tok.left.isInstanceOf[Comment] &&
                state.indentation >= previous.state.column =>
            ""
          case nl: NewlineT
              if nl.acceptSpace &&
                state.indentation >= previous.state.column =>
            " "
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
        lastModification = split.modification
        callback.apply(state, tok, whitespace)
    }
    locations.lastOption.foreach { location =>
      if (debug) logger.debug(s"Total cost: ${location.state.cost}")
    }
  }

  private def isCandidate(location: FormatLocation,
                          style: ScalafmtStyle): Boolean = {
    val token = location.formatToken.right
    val code = token match {
      case c: Comment if isInlineComment(c) => "//"
      case t => t.syntax
    }
    style.alignMap.get(code).exists { ownerRegexp =>
      val owner = getAlignOwner(location.formatToken)
      ownerRegexp.findFirstIn(owner.getClass.getName).isDefined
    }
  }

  def key(token: Token): Int = {
    val ownerKey = {
      val className = owners(token).getClass.getName
      if (style.align.mixedOwners)
        FormatWriter.ownerCategory.getOrElse(className, className)
      else className
    }
    (token.getClass.getName, ownerKey).hashCode()
  }

  private def getAlignOwner(formatToken: FormatToken): Tree =
    formatToken match {
      // Corner case when line ends with comment
      // TODO(olafur) should this be part of owners?
      case FormatToken(x, c: Comment, _) if isInlineComment(c) =>
        owners(x)
      case FormatToken(_, r, _) =>
        owners(r) match {
          case name: Term.Name
              if name.parent.exists(_.isInstanceOf[Term.ApplyInfix]) =>
            name.parent.get
          case x => x
        }
    }

  private def columnsMatch(a: Array[FormatLocation],
                           b: Array[FormatLocation],
                           endOfLine: FormatToken): Int = {
    val result = a.zip(b).takeWhile {
      case (row1, row2) =>
        val row2Owner = getAlignOwner(row2.formatToken)
        val row1Owner = getAlignOwner(row1.formatToken)
        key(row1.formatToken.right) == key(row2.formatToken.right) && {
          val eofParents = parents(owners(endOfLine.right))
          !(eofParents.contains(row1Owner) || eofParents.contains(row2Owner))
        }
    }
    result.length
  }

  /**
    * Returns how many extra spaces are needed to align tokens, as configured
    * by [[style.alignTokens]].
    */
  // TODO(olafur) Refactor implementation to make it maintainable. It's super
  // imperative and error-prone right now.
  def alignmentTokens(locations: Array[FormatLocation],
                      style: ScalafmtStyle): Map[FormatToken, Int] = {
    if (style.align.tokens.isEmpty) Map.empty[FormatToken, Int]
    else {
      val finalResult = Map.newBuilder[FormatToken, Int]
      var i = 0
      var minMatches = Integer.MAX_VALUE
      var block = Vector.empty[Array[FormatLocation]]
      while (i < locations.length) {
        val columnCandidates = Array.newBuilder[FormatLocation]
        while (i < locations.length &&
               !locations(i).split.modification.isNewline) {
          if (isCandidate(locations(i), style)) {
            columnCandidates += locations(i)
          }
          i += 1
        }
        val candidates = columnCandidates.result()
        if (block.isEmpty) {
          if (candidates.nonEmpty &&
              locations(i).split.modification.newlines == 1) {
            block = block :+ candidates
          }
        } else {
          val newlines = locations(i).split.modification.newlines
          val matches =
            columnsMatch(block.last, candidates, locations(i).formatToken)
          minMatches = Math
            .min(minMatches, if (matches > 0) matches else block.head.length)
          if (matches > 0) {
            block = block :+ candidates
          }
          if (matches == 0 || newlines > 1) {
            var column = 0
            val columns = minMatches
            while (column < columns) {
              val blockWithWidth = {
                block.map { line =>
                  val columnWidth = if (column == 0) {
                    line(column).state.column
                  } else {
                    val previousLocation = line(column - 1)
                    val previousColumn =
                      previousLocation.state.column -
                        previousLocation.formatToken.right.syntax.length
                    line(column).state.column - previousColumn
                  }
                  val key =
                    columnWidth - line(column).formatToken.right.syntax.length
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
            if (candidates.isEmpty || newlines > 1) {
              block = Vector.empty[Array[FormatLocation]]
            } else {
              block = Vector(candidates)
            }
            minMatches = Integer.MAX_VALUE
          }
        }
        i += 1
      }
      finalResult.result()
    }
  }
}

object FormatWriter {

  val ownerCategory: Map[String, String] = Map(
    "scala.meta.Defn$Val$DefnValImpl" -> "val/var/def",
    "scala.meta.Defn$Var$DefnVarImpl" -> "val/var/def",
    "scala.meta.Defn$Def$DefnDefImpl" -> "val/var/def",
    "scala.meta.Defn$Class$DefnClassImpl" -> "class/object/trait",
    "scala.meta.Defn$Object$DefnObjectImpl" -> "class/object/trait",
    "scala.meta.Defn$Trait$DefnTraitImpl" -> "class/object/trait"
  )

  case class FormatLocation(formatToken: FormatToken,
                            split: Split,
                            state: State)
}
