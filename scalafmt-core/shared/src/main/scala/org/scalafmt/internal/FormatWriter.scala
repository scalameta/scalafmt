package org.scalafmt.internal

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Mod
import scala.meta.Import
import scala.meta.Importer
import scala.meta.Pkg
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type
import scala.meta.prettyprinters.Syntax
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import java.util.regex.Pattern

import org.scalafmt.internal.FormatWriter.FormatLocation
import org.scalafmt.util.TreeOps

import scala.meta.transversers.Traverser

/**
  * Produces formatted output from sequence of splits.
  */
class FormatWriter(formatOps: FormatOps) {
  import formatOps._
  import org.scalafmt.util.TreeOps._

  def mkString(splits: Vector[Split]): String = {
    val sb = new StringBuilder()
    var lastState = State.start // used to calculate start of formatToken.right.
    reconstructPath(tokens, splits, debug = false) {
      case (state, formatToken, whitespace) =>
        formatToken.left match {
          case c: Comment =>
            sb.append(formatComment(c, state.indentation))
          case token @ Interpolation.Part(_) =>
            sb.append(formatMarginizedString(token, state.indentation))
          case literal @ Constant.String(_) => // Ignore, see below.
          case token =>
            val rewrittenToken =
              formatOps.initStyle.rewriteTokens
                .getOrElse(token.syntax, token.syntax)
            sb.append(rewrittenToken)
        }

        handleTrailingCommasAndWhitespace(formatToken, state, sb, whitespace)

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
      if (comment.syntax.startsWith("/*") &&
        formatOps.initStyle.reformatDocstrings) {
        val isDocstring = comment.syntax.startsWith("/**")
        val spaces: String =
          if (isDocstring && initStyle.scalaDocs) " " * (indent + 2)
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
    if (!initStyle.assumeStandardLibraryStripMargin) token.syntax
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

  def getFormatLocations(
      toks: Array[FormatToken],
      splits: Vector[Split],
      debug: Boolean): Array[FormatLocation] = {
    require(toks.length >= splits.length, "splits !=")
    val statesBuilder = Array.newBuilder[FormatLocation]
    statesBuilder.sizeHint(toks.length)
    var currState = State.start
    splits.zipWithIndex.foreach {
      case (split, i) =>
        val tok = toks(i)
        currState = State.next(currState, styleMap.at(tok), split, tok)
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
      debug: Boolean)(callback: (State, FormatToken, String) => Unit): Unit = {
    require(toks.length >= splits.length, "splits !=")
    val locations = getFormatLocations(toks, splits, debug)
    val tokenAligns = alignmentTokens(locations).withDefaultValue(0)
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
              if (nl.isDouble || isMultilineTopLevelStatement(locations, i))
                "\n\n"
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
    if (debug) {

      locations.lastOption.foreach { location =>
        logger.debug(s"Total cost: ${location.state.cost}")
      }
    }
  }

  lazy val topLevelTokens: List[TokenHash] = {
    val buffer = List.newBuilder[TokenHash]
    val trav = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case Term.Block(_) =>
          ()
        case TreeOps.MaybeTopLevelStat(t) =>
          val tok = leftTok2tok(t.tokens.head)
          val result = leadingComment(prev(tok))
          val hashed = hash(result)
          buffer += hashed
          super.apply(tree)
        case _ =>
          super.apply(tree)
      }
    }

    trav(tree)
    buffer.result()
  }

  private def isMultilineTopLevelStatement(
      toks: Array[FormatLocation],
      i: Int): Boolean = {
    @tailrec def isMultiline(end: Token, i: Int): Boolean = {
      if (i >= toks.length || toks(i).formatToken.left == end) false
      else if (toks(i).split.modification.isNewline) true
      else isMultiline(end, i + 1)
    }
    def actualOwner(token: Token): Tree = owners(token) match {
      case annot: Mod.Annot => annot.parent.get
      case x => x
    }
    initStyle.newlines.alwaysBeforeTopLevelStatements && {
      topLevelTokens.contains(hash(toks(i).formatToken.right)) && {
        val (distance, FormatToken(_, nextNonComment, _)) =
          nextNonCommentWithCount(toks(i).formatToken)
        isMultiline(actualOwner(nextNonComment).tokens.last, i + distance + 1)
      }
    }
  }
  private def isCandidate(location: FormatLocation): Boolean = {
    val token = location.formatToken.right
    val code = token match {
      case c: Comment if isSingleLineComment(c) => "//"
      case t => t.syntax
    }
    styleMap.at(location.formatToken).alignMap.get(code).exists { ownerRegexp =>
      val owner = getAlignOwner(location.formatToken)
      ownerRegexp.findFirstIn(owner.getClass.getName).isDefined
    }
  }

  def key(token: Token): Int = {
    val ownerKey = {
      val treeKind = owners(token).productPrefix
      initStyle.align.treeCategory.getOrElse(treeKind, treeKind)
    }
    val tokenKey = {
      val syntax = token.productPrefix
      initStyle.align.tokenCategory.getOrElse(syntax, syntax)
    }
    (tokenKey, ownerKey).hashCode()
  }

  private def getAlignOwner(formatToken: FormatToken): Tree =
    formatToken match {
      // Corner case when line ends with comment
      // TODO(olafur) should this be part of owners?
      case FormatToken(x, c: Comment, _) if isSingleLineComment(c) =>
        owners(x)
      case FormatToken(_, r, _) =>
        owners(r) match {
          case name: Term.Name
              if name.parent.exists(_.isInstanceOf[Term.ApplyInfix]) =>
            name.parent.get
          case x => x
        }
    }

  private def columnsMatch(
      a: Array[FormatLocation],
      b: Array[FormatLocation],
      endOfLine: FormatToken): Int = {
    val result = a.zip(b).takeWhile {
      case (row1, row2) =>
        val row2Owner = getAlignOwner(row2.formatToken)
        val row1Owner = getAlignOwner(row1.formatToken)
        def sameLengthToRoot =
          vAlignDepth(row1Owner) == vAlignDepth(row2Owner)
        key(row1.formatToken.right) == key(row2.formatToken.right) &&
        sameLengthToRoot && {
          val eofParents = parents(owners(endOfLine.right))
          !(eofParents.contains(row1Owner) || eofParents.contains(row2Owner))
        }
    }
    result.length
  }

  /**
    * Returns how many extra spaces are needed to align tokens, as configured
    * by [[initStyle.align.tokens]].
    */
  // TODO(olafur) Refactor implementation to make it maintainable. It's super
  // imperative and error-prone right now.
  def alignmentTokens(
      locations: Array[FormatLocation]): Map[FormatToken, Int] = {
    if (initStyle.align.tokens.isEmpty || locations.length != tokens.length)
      Map.empty[FormatToken, Int]
    else {
      val finalResult = Map.newBuilder[FormatToken, Int]
      var i = 0
      var minMatches = Integer.MAX_VALUE
      var block = Vector.empty[Array[FormatLocation]]
      while (i < locations.length) {
        val columnCandidates = Array.newBuilder[FormatLocation]
        while (i < locations.length &&
          !locations(i).split.modification.isNewline) {
          if (isCandidate(locations(i))) {
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
          def isEndOfFile = i == locations.length - 1
          if (matches == 0 || newlines > 1 || isEndOfFile) {
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

  import scala.meta.internal.classifiers.classifier

  @classifier
  trait CloseDelim
  object CloseDelim {
    def unapply(token: Token): Boolean =
      token.is[RightParen] || token.is[RightBracket] || token.is[RightBrace]
  }

  private def handleTrailingCommasAndWhitespace(
      formatToken: FormatToken,
      state: State,
      sb: StringBuilder,
      whitespace: String): Unit = {

    import org.scalafmt.config.TrailingCommas

    val owner = owners(formatToken.right)
    if (!runner.dialect.allowTrailingCommas ||
      !isImporterOrDefnOrCallSite(owner)) {
      sb.append(whitespace)
      return
    }

    val left = formatToken.left
    val right = nextNonComment(formatToken).right
    val isNewline = state.splits.last.modification.isNewline

    initStyle.trailingCommas match {
      // foo(
      //   a,
      //   b
      // )
      //
      // Insert a comma after b
      case TrailingCommas.always
          if !left.is[Comma] && !left.is[Comment] &&
            right.is[CloseDelim] && isNewline =>
        sb.append(",")
        sb.append(whitespace)

      // foo(
      //   a,
      //   b // comment
      // )
      //
      // Insert a comma after b (before comment)
      case TrailingCommas.always
          if left.is[Comment] && !prev(formatToken).left.is[Comma] &&
            right.is[CloseDelim] && isNewline =>
        sb.insert(sb.length - left.syntax.length - 1, ",")
        sb.append(whitespace)

      // foo(
      //   a,
      //   b,
      // )
      //
      // Remove the comma after b
      case TrailingCommas.never
          if left.is[Comma] && right.is[CloseDelim] && isNewline =>
        sb.deleteCharAt(sb.length - 1)
        sb.append(whitespace)

      // foo(
      //   a,
      //   b, // comment
      // )
      //
      // Remove the comma after b (before comment)
      case TrailingCommas.never
          if left.is[Comment] && prev(formatToken).left.is[Comma] &&
            right.is[CloseDelim] && isNewline =>
        sb.deleteCharAt(sb.length - left.syntax.length - 2)
        sb.append(whitespace)

      // foo(a, b,)
      //
      // Remove the comma after b
      case _
          if left.is[Comma] && right.is[CloseDelim] &&
            !next(formatToken).left.is[Comment] && !isNewline =>
        sb.deleteCharAt(sb.length - 1)

      case _ => sb.append(whitespace)
    }

  }
}

object FormatWriter {

  case class FormatLocation(
      formatToken: FormatToken,
      split: Split,
      state: State)
}
