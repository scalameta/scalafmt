package org.scalafmt.internal

import java.util.regex.Pattern

import org.scalafmt.util.TreeOps

import scala.annotation.tailrec
import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import scala.meta.tokens.Token
import scala.meta.tokens.{Token => T}
import scala.meta.transversers.Traverser
import scala.meta.{Importer, Mod, Pkg, Term, Tree}

/**
  * Produces formatted output from sequence of splits.
  */
class FormatWriter(formatOps: FormatOps) {
  import FormatWriter._
  import formatOps._

  def mkString(state: State): String = {
    val sb = new StringBuilder()
    var lastState = State.start // used to calculate start of formatToken.right.
    val locations = getFormatLocations(tokens.arr, state, debug = false)

    locations.iterate.foreach { entry =>
      val location = entry.curr
      val state = location.state
      val formatToken = location.formatToken

      formatToken.left match {
        case c: T.Comment =>
          sb.append(formatComment(c, state.indentation))
        case token @ T.Interpolation.Part(_) =>
          sb.append(formatMarginizedString(token, state.indentation))
        case literal: T.Constant.String =>
          sb.append(formatMarginizedString(literal, {
            // compute indentation by locating the previous newline in output
            val sbEnd = sb.length - 1
            2 + sbEnd - sb.lastIndexOf('\n', sbEnd)
          }))
        case c: T.Constant.Long =>
          val syntax = c.syntax
          // longs can be written as hex literals like 0xFF123L. Dont uppercase the X
          if (syntax.startsWith("0x")) {
            sb.append("0x")
            sb.append(initStyle.literals.long.process(syntax.substring(2)))
          } else {
            sb.append(initStyle.literals.long.process(syntax))
          }
        case c: T.Constant.Float =>
          sb.append(initStyle.literals.float.process(c.syntax))
        case c: T.Constant.Double =>
          sb.append(initStyle.literals.double.process(c.syntax))
        case token =>
          val rewrittenToken =
            formatOps.initStyle.rewriteTokens
              .getOrElse(token.syntax, token.syntax)
          sb.append(rewrittenToken)
      }

      entry.formatWhitespace(sb)

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
  private def formatComment(comment: T.Comment, indent: Int): String = {
    val alignedComment =
      if (comment.syntax.startsWith("/*") &&
        formatOps.initStyle.reformatDocstrings) {
        val isDocstring =
          comment.syntax.startsWith("/**") && initStyle.scalaDocs
        val spaces: String =
          getIndentation(if (isDocstring) (indent + 2) else (indent + 1))
        leadingAsteriskSpace
          .matcher(comment.syntax)
          .replaceAll(s"\n$spaces\\*")
      } else {
        comment.syntax
      }
    removeTrailingWhiteSpace(alignedComment)
  }

  val leadingPipeSpace = Pattern.compile("\n *\\|", Pattern.MULTILINE)
  private def formatMarginizedString(token: Token, indent: => Int): String = {
    val shouldMarginize = initStyle.assumeStandardLibraryStripMargin &&
      (token.is[T.Interpolation.Part] || isMarginizedString(token))
    if (shouldMarginize) {
      val firstChar: Char = token match {
        case T.Interpolation.Part(_) =>
          (for {
            parent <- owners(token).parent
            firstInterpolationPart <- parent.tokens.find(
              _.is[T.Interpolation.Part]
            )
            char <- firstInterpolationPart.syntax.headOption
          } yield char).getOrElse(' ')
        case _ =>
          token.syntax.find(_ != '"').getOrElse(' ')
      }
      val extraIndent: Int = if (firstChar == '|') 1 else 0
      val spaces = getIndentation(indent + extraIndent)
      leadingPipeSpace.matcher(token.syntax).replaceAll(s"\n$spaces\\|")
    } else {
      token.syntax
    }
  }

  import org.scalafmt.util.LoggerOps._
  import org.scalafmt.util.TokenOps._

  def getFormatLocations(
      toks: Array[FormatToken],
      state: State,
      debug: Boolean
  ): FormatLocations = {
    require(toks.length >= state.depth, "splits !=")
    val result = new Array[FormatLocation](state.depth)
    var curState = state
    while (curState.depth != 0) {
      val idx = curState.depth - 1
      val tok = toks(idx)
      result(idx) = FormatLocation(tok, curState)
      curState = curState.prev
      if (debug && toks.length < 1000) {
        val left = cleanup(tok.left).slice(0, 15)
        logger.debug(
          f"$left%-15s ${curState.split} ${curState.indentation} ${curState.column}"
        )
      }
    }
    new FormatLocations(result)
  }

  class FormatLocations(val locations: Array[FormatLocation]) {

    val tokenAligns: Map[TokenHash, Int] = alignmentTokens(locations)

    def iterate: Iterator[Entry] =
      Iterator.range(0, locations.length).map(new Entry(_))

    private def getAlign(tok: Token, alignOffset: Int = 0): Int =
      tokenAligns.get(hash(tok)).fold(0)(_ + alignOffset)

    class Entry(val i: Int) {
      val curr = locations(i)
      val previous = locations(math.max(i - 1, 0))

      @inline def tok = curr.formatToken
      @inline def state = curr.state
      @inline def lastModification = previous.state.split.modification

      def getWhitespace(alignOffset: Int): String = {
        // TODO this could get slow for really long comment blocks. If that
        //   becomes a problem, we could also precompute these locations.
        def nextNonComment = {
          val nonCommentIdx =
            locations.indexWhere(!_.formatToken.right.is[T.Comment], i + 1)
          if (0 > nonCommentIdx) None else Some(locations(nonCommentIdx))
        }

        state.split.modification match {
          case Space =>
            val previousAlign =
              if (lastModification != NoSplit) 0
              else getAlign(previous.formatToken.left)
            val currentAlign = getAlign(tok.left, alignOffset)
            getIndentation(1 + currentAlign + previousAlign)

          case nl: NewlineT
              if nl.acceptNoSplit && !tok.left.isInstanceOf[T.Comment] &&
                state.indentation >= previous.state.column =>
            ""

          case nl: NewlineT
              if nl.acceptSpace &&
                state.indentation >= previous.state.column =>
            " "

          case _: NewlineT
              if tok.right.isInstanceOf[T.Comment] &&
                nextNonComment.exists(
                  _.formatToken.right.isInstanceOf[T.Dot]
                ) =>
            // TODO this could slow for really long chains and could be indexed if necessary.
            val prevDotIdx =
              locations.lastIndexWhere(_.formatToken.right.is[T.Dot], i - 1)
            // TODO should this 2 be hard-coded, set to some other existing configurable parameter, or configurable?
            val extraIndent = if (0 <= prevDotIdx) 0 else 2
            "\n" + getIndentation(state.indentation + extraIndent)

          case nl: NewlineT =>
            val newline =
              if (nl.isDouble || isMultilineTopLevelStatement(locations, i))
                "\n\n"
              else "\n"
            if (nl.noIndent) newline
            else newline + getIndentation(state.indentation)

          case Provided(literal) => literal

          case NoSplit => ""
        }
      }

      def formatWhitespace(sb: StringBuilder): Unit = {

        import org.scalafmt.config.TrailingCommas

        val owner = owners(tok.right)
        if (!runner.dialect.allowTrailingCommas ||
          !TreeOps.isImporterOrDefnOrCallSite(owner)) {
          sb.append(getWhitespace(0))
          return
        }
        val isImport = owner.isInstanceOf[Importer]

        val left = tok.left
        val (skip, nextNonCommentTok) = nextNonCommentWithCount(tok)
        val right = nextNonCommentTok.right
        val isNewline =
          Seq(curr, locations(math.min(i + skip, locations.length - 1)))
            .exists(_.state.split.modification.isNewline)

        // Scala syntax allows commas before right braces in weird places,
        // like constructor bodies:
        // def this() = {
        //   this(1),
        // }
        // This code simply ignores those commas because it does not
        // consider them "trailing" commas. It does not remove them
        // in the TrailingCommas.never branch, nor does it
        // try to add them in the TrainingCommas.always branch.
        lazy val rightIsCloseDelim = right
          .is[CloseParenOrBracket] || (right.is[T.RightBrace] && isImport)

        initStyle.trailingCommas match {
          // foo(
          //   a,
          //   b
          // )
          //
          // Insert a comma after b
          case TrailingCommas.always
              if !left.is[T.Comma] &&
                !left.is[T.Comment] &&
                !left.is[T.LeftParen] && // skip empty parentheses
                rightIsCloseDelim && isNewline =>
            sb.append(',').append(getWhitespace(-1))

          // foo(
          //   a,
          //   b,
          // )
          //
          // Remove the comma after b
          case TrailingCommas.never
              if left.is[T.Comma] && rightIsCloseDelim && isNewline =>
            sb.setLength(sb.length - 1)
            sb.append(getWhitespace(1))

          // foo(a, b,)
          //
          // Remove the comma after b
          case _ if left.is[T.Comma] && rightIsCloseDelim && !isNewline =>
            sb.setLength(sb.length - 1)

          case _ => sb.append(getWhitespace(0))
        }
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
          val result = leadingComment(tokens(t.tokens.head, -1))
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
      i: Int
  ): Boolean = {
    @tailrec def isMultiline(end: Token, i: Int): Boolean = {
      if (i >= toks.length || toks(i).formatToken.left == end) false
      else if (toks(i).state.split.modification.isNewline) true
      else isMultiline(end, i + 1)
    }
    def actualOwner(token: Token): Tree = owners(token) match {
      case mod: Mod => mod.parent.get
      case x => x
    }
    initStyle.newlines.alwaysBeforeTopLevelStatements && {
      val formatToken = toks(i).formatToken

      def checkPackage: Option[Boolean] =
        if (!initStyle.activeForEdition_2019_11) None
        else
          Some(owners(formatToken.left))
            .collect { case term: Term.Name => term.parent }
            .flatten
            .collect {
              // package a
              case pkg: Pkg =>
                pkg.stats.headOption

              // package a.b.c
              case select: Term.Select =>
                select.parent.collect { case pkg: Pkg => pkg.stats.headOption }.flatten
            }
            .flatten
            .map {
              case pkg: Pkg => tokens(pkg.ref.tokens.last).right.is[T.LeftBrace]
              case _ => true
            }

      def checkTopLevelStatement: Boolean =
        topLevelTokens.contains(hash(formatToken.right)) && {
          val (distance, FormatToken(_, nextNonComment, _)) =
            nextNonCommentWithCount(formatToken)
          isMultiline(actualOwner(nextNonComment).tokens.last, i + distance + 1)
        }

      checkPackage.getOrElse(checkTopLevelStatement)
    }
  }

  private def isCandidate(location: FormatLocation): Boolean = {
    val token = location.formatToken.right
    val code = token match {
      case c: T.Comment if isSingleLineComment(c) => "//"
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
      case FormatToken(x, c: T.Comment, _) if isSingleLineComment(c) =>
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
      endOfLine: FormatToken
  ): Int = {
    val result = a.zip(b).takeWhile {
      case (row1, row2) =>
        // skip checking if row1 and row2 matches if both of them continues to a single line of comment
        // in order to vertical align adjacent single lines of comment.
        // see: https://github.com/scalameta/scalafmt/issues/1242
        if (isSingleLineComment(row1.formatToken.right) &&
          isSingleLineComment(row2.formatToken.right)) true
        else {
          val row2Owner = getAlignOwner(row2.formatToken)
          val row1Owner = getAlignOwner(row1.formatToken)
          def sameLengthToRoot =
            vAlignDepth(row1Owner) == vAlignDepth(row2Owner)
          key(row1.formatToken.right) == key(row2.formatToken.right) &&
          sameLengthToRoot && {
            val eofParents = TreeOps.parents(owners(endOfLine.right))
            !(eofParents.contains(row1Owner) || eofParents.contains(row2Owner))
          }
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
      locations: Array[FormatLocation]
  ): Map[TokenHash, Int] = {
    if (initStyle.align.tokens.isEmpty || locations.length != tokens.length)
      Map.empty[TokenHash, Int]
    else {
      val finalResult = Map.newBuilder[TokenHash, Int]
      var i = 0
      var minMatches = Integer.MAX_VALUE
      var block = Vector.empty[Array[FormatLocation]]
      while (i < locations.length) {
        val columnCandidates = Array.newBuilder[FormatLocation]
        while (i < locations.length &&
          !locations(i).state.split.modification.isNewline) {
          if (isCandidate(locations(i))) {
            columnCandidates += locations(i)
          }
          i += 1
        }
        val candidates = columnCandidates.result()
        if (block.isEmpty) {
          if (candidates.nonEmpty &&
            locations(i).state.split.modification.newlines == 1) {
            block = block :+ candidates
          }
        } else {
          val newlines = locations(i).state.split.modification.newlines
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
                  key -> hash(line(column).formatToken.left)
                }
              }
              val (maxWidth, _) = blockWithWidth.maxBy(_._1)
              blockWithWidth.foreach {
                case (width, tokenHash) =>
                  finalResult += tokenHash -> (maxWidth - width)
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

  case class FormatLocation(
      formatToken: FormatToken,
      state: State
  )

  // cache indentations to some level
  private val indentations: IndexedSeq[String] = {
    val size = 64
    val buf = new ArrayBuffer[String](size)
    buf += ""
    // use the previous indentation to add another space
    (1 until size).foreach(_ => buf += " " + buf.last)
    buf
  }

  // see if indentation level is cached first
  private def getIndentation(len: Int): String =
    if (len < indentations.length) indentations(len) else " " * len

}
