package org.scalafmt.internal

import java.util.regex.Pattern

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util.TokenOps._
import org.scalafmt.util.{TreeOps, LiteralOps}

import scala.annotation.tailrec
import scala.collection.IndexedSeq
import scala.collection.mutable
import scala.meta.tokens.Token
import scala.meta.tokens.{Token => T}
import scala.meta.transversers.Traverser
import scala.meta.{Importer, Mod, Pkg, Template, Term, Tree, Type}

/**
  * Produces formatted output from sequence of splits.
  */
class FormatWriter(formatOps: FormatOps) {
  import FormatWriter._
  import formatOps._

  def mkString(state: State): String = {
    implicit val style: ScalafmtConfig = initStyle
    val sb = new StringBuilder()
    val locations = getFormatLocations(state)

    locations.iterate.foreach { entry =>
      val location = entry.curr
      val state = location.state.prev // this is the state for left
      val formatToken = location.formatToken

      formatToken.left match {
        // formatting flag fetches from the previous state because of
        // `formatToken.left` rendering. `FormatToken(x, // format: on)` will have
        // formatOff = false, but x still should not be formatted
        case token if state.formatOff => sb.append(token.syntax)
        case c: T.Comment =>
          sb.append(formatComment(c, state.indentation))
        case token @ T.Interpolation.Part(_) =>
          sb.append(formatMarginizedString(token, state.indentation))
        case literal: T.Constant.String =>
          def indent = {
            if (!initStyle.align.stripMargin ||
              state.split.modification.isNewline)
              2 + state.indentation
            else
              2 + state.prev.column + state.prev.split.length
          }
          sb.append(formatMarginizedString(literal, indent))
        case c: T.Constant.Int =>
          sb.append(LiteralOps.prettyPrintInteger(c.syntax))
        case c: T.Constant.Long =>
          sb.append(LiteralOps.prettyPrintInteger(c.syntax))
        case c: T.Constant.Float =>
          sb.append(LiteralOps.prettyPrintFloat(c.syntax))
        case c: T.Constant.Double =>
          sb.append(LiteralOps.prettyPrintDouble(c.syntax))
        case token =>
          val syntax = Option(location.replace).getOrElse(token.syntax)
          val rewrittenToken =
            formatOps.initStyle.rewriteTokens.getOrElse(syntax, syntax)
          sb.append(rewrittenToken)
      }

      entry.formatWhitespace(sb, state.formatOff)
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

  private def getStripMarginPattern(pipe: Char) =
    Pattern.compile(raw"(?<=\n) *(?=[$pipe])", Pattern.MULTILINE)
  private val leadingPipeSpace = getStripMarginPattern('|')

  private def formatMarginizedString(token: Token, indent: => Int): String = {
    val pipeOpt =
      if (!initStyle.assumeStandardLibraryStripMargin) None
      else if (token.is[T.Interpolation.Part]) Some('|')
      else getStripMarginChar(token)
    pipeOpt.fold(token.syntax) { pipe =>
      val pattern =
        if (pipe == '|') leadingPipeSpace else getStripMarginPattern(pipe)
      def firstChar: Char = token match {
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
      val extraIndent =
        if (initStyle.align.stripMargin && firstChar == pipe) 1 else 0
      val spaces = getIndentation(indent + extraIndent)
      pattern.matcher(token.syntax).replaceAll(spaces)
    }
  }

  def getFormatLocations(state: State): FormatLocations = {
    val toks = formatOps.tokens.arr
    require(toks.length >= state.depth, "splits !=")
    val result = new Array[FormatLocation](state.depth)

    @tailrec
    def iter(state: State): Unit =
      if (state.depth != 0) {
        val prev = state.prev
        val idx = prev.depth
        result(idx) = FormatLocation(toks(idx), state)
        iter(prev)
      }
    iter(state)

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

    private implicit val style = initStyle
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
            val isDouble = nl.isDouble ||
              initStyle.newlines.forceBlankBeforeMultilineTopLevelStmt &&
                isMultilineTopLevelStatement(locations, i) ||
              initStyle.newlines.forceBlankAfterMultilineTopLevelStmt &&
                locations.lengthCompare(i + 1) != 0 &&
                topLevelLastToHeadTokens.get(i).exists {
                  isMultilineTopLevelStatement(locations, _)
                }
            val newline = if (isDouble) "\n\n" else "\n"
            if (nl.noIndent) newline
            else newline + getIndentation(state.indentation)

          case Provided(literal) => literal

          case NoSplit => ""
        }
      }

      def formatWhitespace(sb: StringBuilder, formatOff: Boolean): Unit = {

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

          val nextNonCommentTok = nextNonComment(tok)
          val skip = nextNonCommentTok.meta.idx - tok.meta.idx
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

        val noExtraOffset =
          !runner.dialect.allowTrailingCommas ||
            tok.left.is[T.Comment] ||
            formatOff

        if (noExtraOffset)
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

  lazy val (topLevelHeadTokens, topLevelLastToHeadTokens) = {
    val headBuffer = Set.newBuilder[Int]
    val lastBuffer = Map.newBuilder[Int, Int]
    val trav = new Traverser {
      override def apply(tree: Tree): Unit = tree match {
        case _: Term.Block =>
        case t: Template => super.apply(t.stats) // skip inits
        case TreeOps.MaybeTopLevelStat(t) =>
          val leading = leadingComment(tokens(t.tokens.head, -1)).meta.idx
          val trailing = tokens(t.tokens.last).meta.idx
          headBuffer += leading
          lastBuffer += trailing -> leading
          super.apply(tree)
        case _ =>
          super.apply(tree)
      }
    }

    trav(tree)
    (headBuffer.result(), lastBuffer.result())
  }

  private def isMultilineTopLevelStatement(
      toks: Array[FormatLocation],
      i: Int
  ): Boolean = {
    @tailrec def isMultiline(end: Token, i: Int, minLines: Int): Boolean =
      if (minLines <= 0) true
      else if (i >= toks.length || toks(i).formatToken.left == end) false
      else {
        val hasNL = toks(i).state.split.modification.isNewline
        isMultiline(end, i + 1, if (hasNL) minLines - 1 else minLines)
      }
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
      topLevelHeadTokens.contains(formatToken.meta.idx) && {
        val nextNonCommentTok = nextNonComment(formatToken)
        val distance = nextNonCommentTok.meta.idx - formatToken.meta.idx
        val nonCommentOwner = nextNonCommentTok.meta.rightOwner match {
          case mod: Mod => mod.parent.get
          case x => x
        }
        isMultiline(
          nonCommentOwner.tokens.last,
          i + distance + 1,
          initStyle.newlines.topLevelStatementsMinBreaks
        )
      }

    checkPackage.getOrElse(checkTopLevelStatement)
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
    val findParentOnRight =
      TreeOps.findTreeWithParentSimple(endOfLine.meta.rightOwner) _
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
          def isRowOwner(x: Tree) = (x eq row1Owner) || (x eq row2Owner)
          key(row1.formatToken.right) == key(row2.formatToken.right) &&
          sameLengthToRoot && findParentOnRight(isRowOwner).isEmpty
        }
    }
    result.length
  }

  /**
    * Returns how many extra spaces are needed to align tokens, as configured
    * by `initStyle.align.tokens`.
    */
  // TODO(olafur) Refactor implementation to make it maintainable. It's super
  // imperative and error-prone right now.
  def alignmentTokens(
      locations: Array[FormatLocation]
  ): Map[TokenHash, Int] = {
    lazy val noAlignTokens = initStyle.align.tokens.isEmpty &&
      styleMap.tok2style.values.forall(_.align.tokens.isEmpty)
    if (locations.length != tokens.length || noAlignTokens)
      Map.empty[TokenHash, Int]
    else {
      var columnShift = 0
      val finalResult = Map.newBuilder[TokenHash, Int]
      var maxMatches = Integer.MIN_VALUE
      var block = Vector.empty[Array[FormatLocation]]
      val locationIter = new LocationsIterator(locations)
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
          maxMatches = Math
            .max(maxMatches, if (matches > 0) matches else block.head.length)
          if (matches > 0) {
            block = block :+ candidates
          }
          if (matches == 0 || doubleNewline || !locationIter.hasNext) {
            var column = 0
            val columns = maxMatches

            /**
              * Separator length gap needed to align blocks with different token
              * lengths by expression names, not tokens themselves.
              *
              * Without considering gaps:
              * ```
              * libraryDependencies ++= Seq(
              *   "org.scalacheck"  %% "scalacheck" % scalacheckV,
              *   "io.get-coursier" % "interface"   % "0.0.17"
              * )
              * ```
              *
              * Taking gaps into account:
              * ```
              * libraryDependencies ++= Seq(
              *   "org.scalacheck" %% "scalacheck" % scalacheckV,
              *   "io.get-coursier" % "interface"  % "0.0.17"
              * )
              * ```
              * */
            val previousSeparatorLengthGaps = new Array[Int](block.length)
            while (column < columns) {
              val alignmentUnits =
                prepareAlignmentInfo(block, previousSeparatorLengthGaps, column)

              val widest = alignmentUnits.maxBy(_.width)
              alignmentUnits.foreach { info =>
                import info._
                val tokenLengthGap =
                  if (!initStyle.activeForEdition_2020_03) 0
                  else widest.separatorLength - separatorLength
                previousSeparatorLengthGaps(lineIndex) = tokenLengthGap
                finalResult += tokenHash -> (widest.width - width + tokenLengthGap)
              }
              column += 1
            }
            if (candidates.isEmpty || doubleNewline) {
              block = Vector.empty[Array[FormatLocation]]
            } else {
              block = Vector(candidates)
            }
            maxMatches = Integer.MIN_VALUE
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
  ): Vector[AlignmentUnit] = {
    var i = 0
    val units = Vector.newBuilder[AlignmentUnit]
    while (i < block.length) {
      val line = block(i)
      if (column < line.length) {
        val location = line(column)
        val previousWidth =
          if (column == 0) 0 else line(column - 1).shift
        val key = location.shift - previousWidth + separatorLengthGaps(i)
        val separatorLength =
          if (location.formatToken.right.is[Token.Comment]) 0
          else location.formatToken.right.syntax.length
        units += AlignmentUnit(
          key,
          hash(location.formatToken.left),
          separatorLength,
          i
        )
      }
      i += 1
    }

    units.result()
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
    val buf = new mutable.ArrayBuffer[String](size)
    buf += ""
    // use the previous indentation to add another space
    (1 until size).foreach(_ => buf += " " + buf.last)
    buf
  }

  // see if indentation level is cached first
  private def getIndentation(len: Int): String =
    if (len < indentations.length) indentations(len) else " " * len

  /**
    * Because GraalVM native image fails on
    * access to anonymous class fields
    * val locationIter = new Iterator[FormatLocation] {
    *   var last: FormatLocation = null
    *   ...
    * }
    *
    * locationIter.last // RUNTIME ERROR
    *
    * */
  private class LocationsIterator(
      private val locations: Iterable[FormatLocation]
  ) extends Iterator[FormatLocation] {
    private val iter = locations.iterator
    var last: FormatLocation = _
    override def hasNext: Boolean = iter.hasNext
    override def next(): FormatLocation = {
      last = iter.next()
      last
    }
  }
}
