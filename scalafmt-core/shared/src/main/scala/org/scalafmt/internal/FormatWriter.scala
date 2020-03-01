package org.scalafmt.internal

import java.util.regex.Pattern

import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util.TokenOps.TokenHash
import org.scalafmt.util.TreeOps

import scala.annotation.tailrec
import scala.collection.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.meta.Type
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
    val locations = getFormatLocations(state, debug = false)

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
          def indent = {
            val prevState = entry.previous.state
            if (prevState.split.modification.isNewline)
              2 + state.indentation
            else {
              // compute indentation by locating the previous newline in output
              val sbEnd = sb.length - 1;
              2 + sbEnd - sb.lastIndexOf('\n', sbEnd)
            }
          }
          sb.append(formatMarginizedString(literal, indent))
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
          val syntax = Option(location.replace).getOrElse(token.syntax)
          val rewrittenToken =
            formatOps.initStyle.rewriteTokens.getOrElse(syntax, syntax)
          sb.append(rewrittenToken)
      }

      entry.formatWhitespace(sb)
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
      state: State,
      debug: Boolean
  ): FormatLocations = {
    val toks = formatOps.tokens.arr
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

    if (initStyle.rewrite.redundantBraces.parensForOneLineApply
        .getOrElse(initStyle.activeForEdition_2020_01) &&
      initStyle.rewrite.rules.contains(RedundantBraces))
      replaceRedundantBraces(result)

    new FormatLocations(result)
  }

  private def replaceRedundantBraces(locations: Array[FormatLocation]): Unit = {
    // will map closing brace to opening brace and its line offset
    val lookup = mutable.Map.empty[Int, (Int, Int)]
    val inParentheses = initStyle.spaces.inParentheses

    // iterate backwards, to encounter closing braces first
    var lineOffset = 0
    var idx = locations.length - 1
    while (0 <= idx) {
      val loc = locations(idx)
      val tok = loc.formatToken
      val state = loc.state
      // increment line offset
      if (tok.leftHasNewline ||
        state.split.modification.isNewline) lineOffset += 1
      tok.left match {
        case rb: T.RightBrace => // look for "foo { bar }"
          tok.meta.leftOwner match {
            case b: Term.Block if b.parent.exists {
                  case ta: Term.Apply if ta.tokens.last eq rb =>
                    TreeOps.isSingleElement(ta.args, b)
                  case _ => false
                } && RedundantBraces.canRewriteWithParens(b) =>
              val beg = tokens(matching(rb))
              lookup.update(beg.meta.idx, tok.meta.idx -> lineOffset)
            case _ =>
          }
        case _: T.LeftBrace =>
          lookup.remove(idx).foreach {
            case (end, endOffset) if endOffset == lineOffset =>
              // remove space before "{"
              val prevBegState =
                if (0 == idx || (state.prev.split.modification ne Space))
                  state.prev
                else {
                  val prevloc = locations(idx - 1)
                  val prevState = state.prev
                    .copy(split = state.prev.split.copy(modification = NoSplit))
                  locations(idx - 1) = prevloc.copy(
                    shift = prevloc.shift - 1,
                    state = prevState
                  )
                  prevState
                }

              // update "{"
              locations(idx) =
                if (inParentheses)
                  loc.copy(
                    replace = "(",
                    state = state.copy(prev = prevBegState)
                  )
                else {
                  // remove space after "{"
                  val split = state.split.copy(modification = NoSplit)
                  loc.copy(
                    replace = "(",
                    shift = loc.shift - 1,
                    state = state.copy(prev = prevBegState, split = split)
                  )
                }

              val prevEndLoc = locations(end - 1)
              val prevEndState = prevEndLoc.state
              val newPrevEndState =
                if (inParentheses) prevEndState
                else {
                  // remove space before "}"
                  val split = prevEndState.split.copy(modification = NoSplit)
                  val newState = prevEndState.copy(split = split)
                  locations(end - 1) = prevEndLoc
                    .copy(shift = prevEndLoc.shift - 1, state = newState)
                  newState
                }

              // update "}"
              val endLoc = locations(end)
              locations(end) = endLoc.copy(
                replace = ")",
                state = endLoc.state.copy(prev = newPrevEndState)
              )
            case _ =>
          }
        case _ =>
      }
      idx -= 1
    }
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

        /* If we are mutating trailing commas ('always' or 'never'), we should
         * have removed them first in RewriteTrailingCommas; now we simply need
         * to append them in case of 'always', but only when dangling */
        def isClosedDelimWithNewline(expectedNewline: Boolean): Boolean = {
          getClosedDelimWithNewline(expectedNewline).isDefined
        }

        def getClosedDelimWithNewline(
            expectedNewline: Boolean
        ): Option[FormatToken] = {
          def owner = tok.meta.rightOwner

          val (skip, nextNonCommentTok) = nextNonCommentWithCount(tok)
          val right = nextNonCommentTok.right
          def isNewline =
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

          // skip empty parens/braces/brackets
          def rightIsCloseDelim = right match {
            case _: T.RightBrace =>
              !tok.left.is[T.LeftBrace] && owner.is[Importer]
            case _: T.RightParen =>
              !tok.left.is[T.LeftParen] && TreeOps.isDefnOrCallSite(owner)
            case _: T.RightBracket =>
              !tok.left.is[T.LeftBracket] && TreeOps.isDefnOrCallSite(owner)
            case _ => false
          }

          val ok = rightIsCloseDelim && expectedNewline == isNewline
          if (ok) Some(nextNonCommentTok) else None
        }

        @inline def ws(offset: Int): Unit = sb.append(getWhitespace(offset))

        if (!runner.dialect.allowTrailingCommas || tok.left.is[T.Comment])
          ws(0)
        else
          initStyle.trailingCommas match {
            // remove comma if no newline
            case TrailingCommas.preserve
                if tok.left.is[T.Comma] && isClosedDelimWithNewline(false) =>
              sb.setLength(sb.length - 1)
              if (!tok.right.is[T.RightParen]) ws(1)
              else if (initStyle.spaces.inParentheses) sb.append(' ')
            // append comma if newline
            case TrailingCommas.always
                if !tok.left.is[T.Comma] && isClosedDelimWithNewline(true) =>
              sb.append(',')
              ws(-1)
            // append comma if newline and multiple args
            case TrailingCommas.multiple
                if !tok.left.is[T.Comma] && getClosedDelimWithNewline(true)
                  .exists(isCloseDelimForTrailingCommasMultiple) =>
              sb.append(',')
              ws(-1)
            case _ => ws(0)
          }
      }
    }
  }

  private def isCloseDelimForTrailingCommasMultiple(ft: FormatToken): Boolean =
    ft.meta.rightOwner match {
      case x: Importer => x.importees.length > 1
      case _ =>
        val args = getApplyArgs(ft, true)(initStyle)._2
        args.length > 1 && (args.last match {
          case _: Term.Repeated => false
          case t: Term.Param => !t.decltpe.exists(_.is[Type.Repeated])
          case _ => true
        })
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
          Some(formatToken.meta.leftOwner)
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
    // Corner case when line ends with comment
    // TODO(olafur) should this be part of owners?
    if (isSingleLineComment(formatToken.right))
      formatToken.meta.leftOwner
    else
      formatToken.meta.rightOwner match {
        case name: Term.Name
            if name.parent.exists(_.isInstanceOf[Term.ApplyInfix]) =>
          name.parent.get
        case x => x
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
          sameLengthToRoot &&
          TreeOps
            .findTreeWithParent(endOfLine.meta.rightOwner) {
              case `row1Owner` | `row2Owner` => Some(true)
              case _ => None
            }
            .isEmpty
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
      var columnShift = 0
      val finalResult = Map.newBuilder[TokenHash, Int]
      var minMatches = Integer.MAX_VALUE
      var block = Vector.empty[Array[FormatLocation]]
      val locationIter = new Iterator[FormatLocation] {
        private val iter = locations.iterator
        var last: FormatLocation = null
        override def hasNext: Boolean = iter.hasNext
        override def next(): FormatLocation = {
          last = iter.next()
          last
        }
      }
      while (locationIter.hasNext) {
        val columnCandidates = Array.newBuilder[FormatLocation]
        def shouldContinue(floc: FormatLocation): Boolean = {
          val ok = !floc.state.split.modification.isNewline
          if (!ok || floc.formatToken.leftHasNewline) columnShift = 0
          columnShift += floc.shift
          if (ok)
            if (isCandidate(floc))
              columnCandidates += floc.copy(shift =
                floc.state.prev.column + columnShift
              )
          ok
        }
        while (shouldContinue(locationIter.next()) && locationIter.hasNext) {}
        val location = locationIter.last
        val candidates = columnCandidates.result()
        val doubleNewline = location.state.split.modification.newlines > 1
        if (block.isEmpty) {
          if (candidates.nonEmpty && !doubleNewline)
            block = block :+ candidates
        } else {
          val matches =
            columnsMatch(block.last, candidates, location.formatToken)
          minMatches = Math
            .min(minMatches, if (matches > 0) matches else block.head.length)
          if (matches > 0) {
            block = block :+ candidates
          }
          if (matches == 0 || doubleNewline || !locationIter.hasNext) {
            var column = 0
            val columns = minMatches

            /**
              * Separator length gap needed for align blocks with different token
              * lengths by expression names, not tokens itself.
              *
              * Without gaps considering:
              * ```
              * libraryDependencies ++= Seq(
              *   "org.scalacheck"  %% "scalacheck" % scalacheckV,
              *   "io.get-coursier" % "interface"   % "0.0.17"
              * )
              * ```
              *
              * With gaps considering:
              * ```
              * libraryDependencies ++= Seq(
              *   "org.scalacheck" %% "scalacheck" % scalacheckV,
              *   "io.get-coursier" % "interface"  % "0.0.17"
              * )
              * ```
              * */
            val previousSeparatorLengthGaps = Array.fill(block.length)(0)
            while (column < columns) {
              val alignmentUnits =
                prepareAlignmentInfo(block, previousSeparatorLengthGaps, column)

              val maxWidth = alignmentUnits.map(_.width).max

              val maxWidthTokenLength =
                alignmentUnits.maxBy(_.width).separatorLength

              alignmentUnits.foreach { info =>
                import info._
                val tokenLengthGap = maxWidthTokenLength - separatorLength
                previousSeparatorLengthGaps(lineIndex) = tokenLengthGap
                finalResult += tokenHash -> (maxWidth - width + tokenLengthGap)
              }
              column += 1
            }
            if (candidates.isEmpty || doubleNewline) {
              block = Vector.empty[Array[FormatLocation]]
            } else {
              block = Vector(candidates)
            }
            minMatches = Integer.MAX_VALUE
          }
        }
      }
      finalResult.result()
    }
  }

  private def prepareAlignmentInfo(
      block: Vector[Array[FormatLocation]],
      separatorLengthGaps: Array[Int],
      column: Int
  ): Vector[AlignmentUnit] =
    block.zipWithIndex.map {
      case (line, i) =>
        val location = line(column)
        val previousWidth =
          if (column == 0) 0 else line(column - 1).shift
        val key = location.shift - previousWidth + separatorLengthGaps(i)
        val separatorLength =
          if (location.formatToken.right.is[Token.Comment]) 0
          else location.formatToken.right.text.length
        AlignmentUnit(
          key,
          hash(location.formatToken.left),
          separatorLength,
          i
        )
    }

}

object FormatWriter {

  case class FormatLocation(
      formatToken: FormatToken,
      state: State,
      shift: Int = 0,
      replace: String = null
  )

  /**
    * Alignment information extracted from FormatToken. Used only when align!=none.
    * For example:
    * ```
    * libraryDependencies ++= Seq(
    *   "io.get-coursier" % "interface" % "0.0.17",
    *   "org.scalacheck" %% "scalacheck" % scalacheckV
    * )
    * ```
    *
    * `"io.get-coursier" % "interface" % "0.0.17"`
    *  |<--------------->|      => width
    *  hash("io.get-coursier")  => tokenHash
    *  length(%)                => separatorLength
    *  line number in block (1) => lineIndex
    * */
  case class AlignmentUnit(
      width: Int,
      tokenHash: TokenHash,
      separatorLength: Int,
      lineIndex: Int
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
