package org.scalafmt.internal

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.Comments
import org.scalafmt.config.Docstrings
import org.scalafmt.config.FormatEvent
import org.scalafmt.config.Newlines
import org.scalafmt.config.RewriteScala3Settings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.TrailingCommas
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util.LiteralOps
import org.scalafmt.util.TokenOps._
import org.scalafmt.util.TreeOps

import scala.meta._
import scala.meta.internal.Scaladoc
import scala.meta.internal.parsers.ScaladocParser
import scala.meta.tokens.{Token => T}
import scala.meta.transversers.Traverser

import java.nio.CharBuffer
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.mutable

/** Produces formatted output from sequence of splits.
  */
class FormatWriter(formatOps: FormatOps) {
  import FormatWriter._
  import formatOps._

  def mkString(state: State): String = {
    implicit val sb = new StringBuilder()
    val locations = getFormatLocations(state)
    styleMap.init.runner.event(FormatEvent.Written(locations))

    var delayedAlign = 0
    locations.foreach { entry =>
      val location = entry.curr
      implicit val style: ScalafmtConfig = location.style
      val formatToken = location.formatToken
      var skipWs = false

      formatToken.left match {
        case _ if entry.previous.formatToken.meta.formatOff =>
          sb.append(formatToken.meta.left.text) // checked the state for left
        case _: T.Comment => entry.formatComment
        case _: T.Interpolation.Part | _: T.Constant.String => sb
            .append(entry.formatMarginized)
        case _: T.Constant.Int => sb
            .append(LiteralOps.prettyPrintInteger(formatToken.meta.left.text))
        case _: T.Constant.Long => sb
            .append(LiteralOps.prettyPrintInteger(formatToken.meta.left.text))
        case _: T.Constant.Float => sb
            .append(LiteralOps.prettyPrintFloat(formatToken.meta.left.text))
        case _: T.Constant.Double => sb
            .append(LiteralOps.prettyPrintDouble(formatToken.meta.left.text))
        case _ =>
          val syntax = Option(location.replace)
            .getOrElse(formatToken.meta.left.text)
          val rewrittenToken = style.rewriteTokens.getOrElse(syntax, syntax)
          sb.append(rewrittenToken)
      }

      location.optionalBraces.toSeq.sortBy { case (indent, _) => -indent }
        .foreach { case (indent, owner) =>
          val label = getEndMarkerLabel(owner)
          if (label != null) {
            val numBlanks = locations
              .getBlanks(owner, owner, locations.getNest(owner))
              .fold(0) { case (blanks, _, last) =>
                val numBlanks = blanks.beforeEndMarker
                if (numBlanks > 0) numBlanks
                else locations.extraBlankTokens.get(last.meta.idx)
                  .fold(0)(x => if (x > 0) 1 else 0)
              }
            sb.append(getNewlines(numBlanks)).append(getIndentation(indent))
              .append("end ").append(label)
          }
        }

      // missing braces
      if (location.missingBracesIndent.nonEmpty) {
        location.missingBracesIndent.toSeq.sorted(Ordering.Int.reverse)
          .foreach(i => sb.append('\n').append(getIndentation(i)).append("}"))
        if (location.missingBracesOpenOrTuck) {
          skipWs = true
          sb.append(" ")
        } else if (formatToken.right.is[T.RightParen]) skipWs = true
      } else if (location.missingBracesOpenOrTuck) sb.append(" {")

      if (!skipWs) delayedAlign = entry.formatWhitespace(delayedAlign)
    }

    sb.toString()
  }

  private def getFormatLocations(state: State): FormatLocations = {
    val toks = tokens.arr
    require(toks.length >= state.depth, "splits !=")
    val result = new Array[FormatLocation](state.depth)

    @tailrec
    def iter(state: State, lineId: Int): Unit = if (state.depth != 0) {
      val prev = state.prev
      val idx = prev.depth
      val ft = toks(idx)
      val newlines =
        if (idx == 0) 0
        else state.split.modExt.mod.newlines + ft.meta.left.countNL
      val newLineId = lineId + newlines
      result(idx) = FormatLocation(ft, state, styleMap.at(ft), newLineId)
      iter(prev, newLineId)
    }
    iter(state, 0)

    if (state.depth == toks.length) { // format completed
      val initStyle = styleMap.init
      if (initStyle.dialect.allowSignificantIndentation) {
        if (initStyle.rewrite.scala3.removeEndMarkerMaxLines > 0)
          checkRemoveEndMarkers(result)
        if (initStyle.rewrite.scala3.insertEndMarkerMinLines > 0)
          checkInsertEndMarkers(result)
      }
      if (initStyle.rewrite.insertBraces.minLines > 0) checkInsertBraces(result)
      if (
        initStyle.rewrite.rules.contains(RedundantBraces) &&
        initStyle.rewrite.redundantBraces.parensForOneLineApply
      ) replaceRedundantBraces(result)
    }

    new FormatLocations(result)
  }

  private def replaceRedundantBraces(locations: Array[FormatLocation]): Unit = {
    // will map closing brace to opening brace and its line offset
    val lookup = mutable.Map.empty[Int, (Int, Int)]

    def checkApply(t: Tree): Boolean = t.parent match {
      case Some(p @ Term.ArgClause(`t` :: Nil, _)) => TreeOps.isParentAnApply(p)
      case _ => false
    }

    // iterate backwards, to encounter closing braces first
    var idx = locations.length - 1
    while (0 <= idx) {
      val loc = locations(idx)
      val tok = loc.formatToken
      tok.left match {
        case rb: T.RightBrace => // look for "foo { bar }"
          val ok = tok.meta.leftOwner match {
            case b: Term.Block => checkApply(b) &&
              RedundantBraces.canRewriteBlockWithParens(b) &&
              b.parent.exists(tokens.getLast(_) eq tok)
            case f: Term.FunctionTerm => checkApply(f) &&
              RedundantBraces.canRewriteFuncWithParens(f)
            case t @ TreeOps.SingleArgInBraces(_, arg, _) => TreeOps
                .isParentAnApply(t) &&
              RedundantBraces.canRewriteStatWithParens(arg)
            case _ => false
          }
          if (ok) {
            val beg = tokens(tokens.matching(rb))
            lookup.update(beg.meta.idx, tok.meta.idx -> loc.leftLineId)
          }
        case _: T.LeftBrace =>
          val state = loc.state
          val style = loc.style
          lookup.remove(idx).foreach {
            case (end, endOffset)
                if endOffset == loc.leftLineId &&
                  (style.rewrite.trailingCommas.allowFolding ||
                    style.getTrailingCommas != TrailingCommas.always) =>
              val inParentheses = style.spaces.inParentheses
              // remove space before "{"
              val prevBegState =
                if (0 == idx || (state.prev.split.modExt.mod ne Space))
                  state.prev
                else {
                  val prevloc = locations(idx - 1)
                  val prevState = state.prev
                    .copy(split = state.prev.split.withMod(NoSplit))
                  locations(idx - 1) = prevloc
                    .copy(shift = prevloc.shift - 1, state = prevState)
                  prevState
                }

              // update "{"
              locations(idx) =
                if (inParentheses) loc
                  .copy(replace = "(", state = state.copy(prev = prevBegState))
                else {
                  // remove space after "{"
                  val split = state.split.withMod(NoSplit)
                  loc.copy(
                    replace = "(",
                    shift = loc.shift - 1,
                    state = state.copy(prev = prevBegState, split = split),
                  )
                }

              val prevEndLoc = locations(end - 1)
              val prevEndState = prevEndLoc.state
              val newPrevEndState =
                if (inParentheses) prevEndState
                else {
                  // remove space before "}"
                  val split = prevEndState.split.withMod(NoSplit)
                  val newState = prevEndState.copy(split = split)
                  locations(end - 1) = prevEndLoc
                    .copy(shift = prevEndLoc.shift - 1, state = newState)
                  newState
                }

              // update "}"
              val endLoc = locations(end)
              locations(end) = endLoc.copy(
                replace = ")",
                state = endLoc.state.copy(prev = newPrevEndState),
              )
            case _ =>
          }
        case _ =>
      }
      idx -= 1
    }
  }

  private def getOptionalBracesOwner(
      floc: FormatLocation,
      minBlockStats: Int,
  ): Option[Tree] = {
    val ob = formatOps.OptionalBraces.get(floc.formatToken)(floc.style)
    ob.flatMap(_.owner).filter {
      /* if we add the end marker, it might turn a single-stat expression (or
       * block) into a multi-stat block and thus potentially change how that
       * parent expression would have been formatted; so, avoid those cases.
       */
      _.parent match {
        case Some(t: Term.Block) => t.stats.lengthCompare(minBlockStats) >= 0
        case Some(_: Template | _: Source | _: Pkg) => true
        case _ => false
      }
    }
  }

  private def checkRemoveEndMarkers(locations: Array[FormatLocation]): Unit = {
    var removedLines = 0
    val endMarkers = new mutable.ListBuffer[(Int, Int)]
    locations.foreach { x =>
      val idx = x.formatToken.meta.idx
      val floc =
        if (removedLines > 0 && x.isNotRemoved) {
          val floc = x.copy(leftLineId = x.leftLineId + removedLines)
          locations(idx) = floc
          floc
        } else x
      if (endMarkers.nonEmpty && endMarkers(0)._1 == idx) {
        val begIdx = endMarkers.remove(0)._2
        val endIdx = locations.lastIndexWhere(_.isNotRemoved, idx)
        if (endIdx >= 0) {
          val bLoc = locations(begIdx)
          val eLoc = locations(endIdx)
          val span = getLineDiff(bLoc, eLoc)
          if (span < bLoc.style.rewrite.scala3.removeEndMarkerMaxLines) {
            val loc2 = locations(idx + 2)
            locations(idx + 1) = locations(idx + 1).remove
            locations(idx + 2) = loc2.remove
            locations(endIdx) = eLoc.copy(state = loc2.state)
            removedLines += 1
          }
        }
      } else getOptionalBracesOwner(floc, 3).foreach { owner =>
        // do not skip comment lines, as the parser doesn't handle comments
        // at end of optional braces region and treats them as outside
        val endFt = tokens.nextNonCommentSameLine(tokens.getLast(owner))
        val ok = endFt.meta.rightOwner match {
          case em: Term.EndMarker => em.parent == owner.parent
          case _ => false
        }
        if (ok) {
          // "<left> end name <right>"
          val end = endFt.meta.idx
          val isStandalone = locations(end).hasBreakAfter &&
            end + 2 < locations.length && locations(end + 2).hasBreakAfter
          if (isStandalone) {
            val settings = floc.style.rewrite.scala3
            val idx = settings.countEndMarkerLines match {
              case RewriteScala3Settings.EndMarkerLines.lastBlockOnly => tokens
                  .nextNonCommentSameLine(floc.formatToken).meta.idx + 1
              case RewriteScala3Settings.EndMarkerLines.all => tokens
                  .getHead(owner).meta.idx
            }
            endMarkers.prepend(end -> idx)
          }
        }
      }
    }
  }

  private def checkInsertEndMarkers(locations: Array[FormatLocation]): Unit =
    locations.foreach { floc =>
      getOptionalBracesOwner(floc, 2).foreach { owner =>
        val endFt = tokens.getLast(owner)
        val ok = tokens.nextNonComment(endFt).meta.rightOwner match {
          case em: Term.EndMarker => em.parent != owner.parent
          case _ => true
        }
        if (ok) {
          val end = endFt.meta.idx
          val eLoc = locations(end)
          val bLoc = locations(tokens.getHead(owner).meta.idx)
          val begIndent = bLoc.state.prev.indentation
          def appendOwner() = locations(end) = eLoc
            .copy(optionalBraces = eLoc.optionalBraces + (begIndent -> owner))
          def removeOwner() = locations(end) = eLoc
            .copy(optionalBraces = eLoc.optionalBraces - begIndent)
          def processOwner() = {
            val settings = floc.style.rewrite.scala3
            def okSpan(loc: FormatLocation) = 1 + getLineDiff(loc, eLoc) >=
              settings.insertEndMarkerMinLines
            settings.countEndMarkerLines match {
              case RewriteScala3Settings.EndMarkerLines.lastBlockOnly =>
                val i = tokens.nextNonCommentSameLine(floc.formatToken).meta.idx
                if (okSpan(locations(i + 1))) appendOwner() else removeOwner()
              case RewriteScala3Settings.EndMarkerLines.all =>
                if (!eLoc.optionalBraces.contains(begIndent) && okSpan(bLoc))
                  appendOwner()
            }
          }
          if (eLoc.hasBreakAfter) processOwner()
        }
      }
    }

  private def checkInsertBraces(locations: Array[FormatLocation]): Unit = {
    def checkInfix(tree: Tree): Boolean = tree match {
      case ai: Term.ApplyInfix => tokens.isEnclosedInParens(ai) ||
        tokens.prevNonCommentSameLine(tokens.tokenJustBefore(ai.op)).noBreak &&
        checkInfix(ai.lhs) &&
        (ai.argClause.values match {
          case head :: Nil => checkInfix(head)
          case _ => true
        })
      case _ => true
    }
    var addedLines = 0
    val willAddLines = new mutable.ListBuffer[Int]
    locations.foreach { x =>
      val idx = x.formatToken.meta.idx
      val floc =
        if (addedLines > 0 && x.isNotRemoved) {
          val floc = x.copy(leftLineId = x.leftLineId - addedLines)
          locations(idx) = floc
          floc
        } else x
      if (willAddLines.nonEmpty && willAddLines(0) == idx) {
        addedLines += 1
        willAddLines.remove(0)
      }
      @tailrec
      def hasBreakAfter(i: Int): Boolean = i < locations.length && {
        val x = locations(i)
        if (!x.isNotRemoved) hasBreakAfter(i + 1)
        else if (x.hasBreakAfter) true
        else if (!x.formatToken.right.is[T.Comment]) false
        else hasBreakAfter(i + 1)
      }
      @tailrec
      def noAnnoFor(tree: Tree): Boolean = tree.parent match {
        case Some(p @ (_: Term | _: Term.ArgClause)) => noAnnoFor(p)
        case Some(p: Init) => !p.parent.exists(_.is[Mod.Annot])
        case _ => true
      }
      val style = floc.style
      val ib = style.rewrite.insertBraces
      val ft = floc.formatToken
      val ok = !ft.meta.formatOff && ib.minLines > 0 &&
        floc.missingBracesIndent.isEmpty
      val mb =
        if (ok) formatOps.MissingBraces.getBlocks(ft, ib.allBlocks)
          .filter { case (y, _) =>
            checkInfix(y) && hasBreakAfter(idx) && noAnnoFor(y)
          }
        else None
      mb.foreach { case (owner, otherBlocks) =>
        val endFt = tokens.nextNonCommentSameLine(tokens.getLast(owner))
        val end = endFt.meta.idx
        val eLoc = locations(end)
        val begIndent = floc.state.prev.indentation
        def checkSpan: Boolean =
          getLineDiff(floc, eLoc) + addedLines >= ib.minLines ||
            otherBlocks.exists { case (b, e) =>
              val bIdx = tokens.tokenJustBefore(b).meta.idx
              val eIdx = tokens.getLast(e).meta.idx
              val span = getLineDiff(locations(bIdx), locations(eIdx))
              ib.minLines <=
                (if (bIdx <= idx && eIdx > idx) span + addedLines else span)
            }
        if (
          !endFt.meta.formatOff && eLoc.hasBreakAfter &&
          !eLoc.missingBracesIndent.contains(begIndent) && checkSpan
        ) {
          val addLine = style.newlines.alwaysBeforeElseAfterCurlyIf ||
            (endFt.right match {
              case _: T.KwElse | _: T.KwCatch | _: T.KwFinally =>
                !owner.parent.contains(endFt.meta.rightOwner)
              case _ => true
            })
          if (addLine) willAddLines.prepend(end)
          locations(idx) = floc.copy(missingBracesOpenOrTuck = true)
          locations(end) = eLoc.copy(
            missingBracesOpenOrTuck = !addLine &&
              (eLoc.missingBracesIndent.isEmpty || eLoc.missingBracesOpenOrTuck),
            missingBracesIndent = eLoc.missingBracesIndent + begIndent,
          )
        }
      }
    }
  }

  class FormatLocations(val locations: Array[FormatLocation]) {

    val tokenAligns: Map[Int, Int] = alignmentTokens

    def foreach(f: Entry => Unit): Unit = Iterator.range(0, locations.length)
      .foreach { i =>
        val entry = new Entry(i)
        if (entry.curr.isNotRemoved) f(entry)
      }

    class Entry(val i: Int) {
      val curr = locations(i)
      private implicit val style: ScalafmtConfig = curr.style
      def previous = locations(math.max(i - 1, 0))

      @inline
      def tok = curr.formatToken
      @inline
      def state = curr.state
      @inline
      def prevState = curr.state.prev

      private def appendWhitespace(alignOffset: Int, delayedAlign: Int)(implicit
          sb: StringBuilder,
      ): Int = {
        val mod = state.split.modExt.mod
        def currentAlign = tokenAligns.get(i).fold(0)(_ + alignOffset)
        val ws = mod match {
          case nl: NewlineT =>
            val extraBlanks =
              if (i == locations.length - 1) 0
              else extraBlankTokens.getOrElse(i, if (nl.isDouble) 1 else 0)
            val newlines = getNewlines(extraBlanks)
            if (nl.noIndent) newlines
            else newlines + getIndentation(state.indentation)

          case p: Provided => p.betweenText

          case NoSplit if style.align.delayUntilSpace =>
            return delayedAlign + currentAlign // RETURNING!

          case _ => getIndentation(mod.length + currentAlign + delayedAlign)
        }
        sb.append(ws)
        0
      }

      def formatWhitespace(delayedAlign: Int)(implicit sb: StringBuilder): Int = {

        import org.scalafmt.config.TrailingCommas

        /* If we are mutating trailing commas ('always' or 'never'), we should
         * have removed them first in RewriteTrailingCommas; now we simply need
         * to append them in case of 'always', but only when dangling */
        def isClosedDelimWithNewline(expectedNewline: Boolean): Boolean =
          getClosedDelimWithNewline(expectedNewline).isDefined

        def getClosedDelimWithNewline(whenNL: Boolean): Option[FormatToken] = {
          @tailrec
          def iter(
              floc: FormatLocation,
              hadNL: Boolean,
          ): Option[FormatToken] = {
            val isNL = floc.hasBreakAfter
            if (isNL && !whenNL) None
            else {
              val ft = floc.formatToken
              def gotNL = hadNL || isNL
              ft.right match {
                case _: T.Comment =>
                  val idx = ft.meta.idx + 1
                  if (idx == locations.length) None
                  else iter(locations(idx), gotNL)
                case _ =>
                  val ok = gotNL == whenNL &&
                    TreeOps
                      .rightIsCloseDelimForTrailingComma(tok.left, ft, whenNL)
                  if (ok) Some(ft) else None
              }
            }
          }

          iter(curr, false)
        }

        @inline
        def ws(offset: Int) = appendWhitespace(offset, delayedAlign)

        val noExtraOffset = !dialect.allowTrailingCommas ||
          tok.left.is[T.Comment] || previous.formatToken.meta.formatOff

        if (noExtraOffset) ws(0)
        else style.getTrailingCommas match {
          // remove comma if no newline
          case TrailingCommas.keep
              if tok.left.is[T.Comma] &&
                (isClosedDelimWithNewline(false) ||
                  (tok.meta.leftOwner match {
                    // closing paren could have been removed by rewrite
                    case x: Term.ArgClause => tokens.getLast(x) eq tok
                    case _ => false
                  })) =>
            sb.setLength(sb.length - 1)
            if (!tok.right.is[T.RightParen]) ws(1)
            else if (style.spaces.inParentheses) {
              sb.append(getIndentation(1 + delayedAlign)); 0
            } else delayedAlign
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

      def formatMarginized: String = {
        val text = tok.meta.left.text
        val tupleOpt = tok.left match {
          case _ if !style.assumeStandardLibraryStripMargin => None
          case _ if !tok.meta.left.hasNL => None
          case _: T.Constant.String => TreeOps
              .getStripMarginChar(tok.meta.leftOwner).map { pipe =>
                def isPipeFirstChar = text.find(_ != '"').contains(pipe)
                val noAlign = !style.align.stripMargin || curr.hasBreakBefore
                val thisOffset =
                  if (style.align.stripMargin) if (isPipeFirstChar) 3 else 2
                  else style.indent.main
                val prevIndent =
                  if (noAlign) prevState.indentation
                  else prevState.prev.column + prevState.prev.split.length
                (pipe, thisOffset + prevIndent)
              }
          case _: T.Interpolation.Part => TreeOps
              .getStripMarginCharForInterpolate(tok.meta.leftOwner).map {
                val alignPipeOffset = if (style.align.stripMargin) 1 else 0
                (_, prevState.indentation + alignPipeOffset)
              }
          case _ => None
        }
        tupleOpt.fold(text) { case (pipe, indent) =>
          val spaces = getIndentation(indent)
          getStripMarginPattern(pipe).matcher(text).replaceAll(spaces)
        }
      }

      def formatComment(implicit sb: StringBuilder): Unit = {
        val text = tok.meta.left.text
        if (text.startsWith("//")) new FormatSlc(text).format()
        else if (text == "/**/") sb.append(text)
        else if (isDocstring(text)) formatDocstring(text)
        else new FormatMlc(text).format()
      }

      private def formatOnelineDocstring(
          text: String,
      )(implicit sb: StringBuilder): Boolean = curr.isStandalone && {
        val matcher = onelineDocstring.matcher(text)
        matcher.matches() &&
        (style.docstrings.oneline match {
          case Docstrings.Oneline.fold => true
          case Docstrings.Oneline.unfold => false
          case Docstrings.Oneline.keep => matcher.start(1) == -1 &&
            matcher.start(3) == -1
        }) && {
          val content = matcher.group(2)
          val folding = style.docstrings.wrap match {
            case Docstrings.Wrap.yes => content.length <= // 7 is the length of "/** " and " */"
                style.docstringsWrapMaxColumn - prevState.indentation - 7
            case _ => true
          }
          if (folding) sb.append("/** ").append(content).append(" */")
          folding
        }
      }

      private def formatDocstring(
          text: String,
      )(implicit sb: StringBuilder): Unit =
        if (style.docstrings.style eq Docstrings.Preserve) sb.append(text)
        else if (!formatOnelineDocstring(text)) new FormatMlDoc(text).format()

      private abstract class FormatCommentBase(
          protected val maxColumn: Int,
          protected val extraIndent: Int = 1,
          protected val leadingMargin: Int = 0,
      )(implicit sb: StringBuilder) {
        protected final val breakBefore = curr.hasBreakBefore
        protected final val indent = prevState.indentation
        // extra 1 is for "*" (in "/*" or " *") or "/" (in "//")
        protected final val maxLength = maxColumn - indent - extraIndent - 1

        protected final def getFirstLineLength =
          if (breakBefore) leadingMargin
          else prevState.prev.column - prevState.prev.indentation +
            prevState.split.length

        protected final def canRewrite = style.comments.wrap match {
          case Comments.Wrap.no => false
          case Comments.Wrap.trailing => curr.hasBreakAfter
          case Comments.Wrap.standalone => breakBefore && curr.hasBreakAfter
        }

        protected final type WordIter = Iterator[String]

        protected class WordFormatter(
            appendLineBreak: () => Unit,
            extraMargin: String = " ",
            prefixFirstWord: String => String = _ => "",
        ) {
          final def apply(
              iter: WordIter,
              lineLength: Int,
              atLineBeg: Boolean,
              needSpaceIfAtLineBeg: Boolean = true,
          ): Int = iterate(
            iter,
            sb.length - lineLength,
            0,
            atLineBeg,
            needSpaceIfAtLineBeg,
          )

          @tailrec
          private def iterate(
              iter: WordIter,
              lineBeg: Int,
              linesSoFar: Int,
              atLineBeg: Boolean = false,
              needSpaceIfAtLineBeg: Boolean = false,
          ): Int =
            if (iter.hasNext) {
              val word = iter.next()
              var lines = linesSoFar
              var nextLineBeg = lineBeg
              def firstWordPrefix = prefixFirstWord(word)
              def nextLineLength = 1 + word.length + sb.length - lineBeg
              if (atLineBeg) {
                if (needSpaceIfAtLineBeg) sb.append(' ')
                sb.append(firstWordPrefix)
              } else if (nextLineLength <= maxLength) sb.append(' ')
              else {
                appendLineBreak()
                lines += 1
                nextLineBeg = sb.length
                sb.append(extraMargin)
                sb.append(firstWordPrefix)
              }
              sb.append(word)
              iterate(iter, nextLineBeg, lines)
            } else linesSoFar
        }

        protected def terminateMlc(begpos: Int, lines: Int): Unit =
          if (lines == 0 && style.comments.wrapSingleLineMlcAsSlc) sb
            .setCharAt(begpos - 1, '/')
          else sb.append(" */")

        protected def append(csq: CharSequence, beg: Int, end: Int) = sb
          .append(CharBuffer.wrap(csq, beg, end))

      }

      private class FormatSlc(text: String)(implicit sb: StringBuilder)
          extends FormatCommentBase(style.maxColumn) {
        def format(): Unit = {
          val trimmed = removeTrailingWhiteSpace(text)
          val isCommentedOut = prevState.split.modExt.mod match {
            case m: NewlineT if m.noIndent => true
            case _ => indent == 0
          }
          if (isCommentedOut) sb.append(trimmed)
          else {
            val nonSlash = trimmed.indexWhere(_ != '/')
            val hasSpace = nonSlash < 0 || // else space not needed
              Character.isWhitespace(trimmed.charAt(nonSlash))
            val column = prevState.column - text.length + trimmed.length +
              (if (hasSpace) 0 else 1)
            if (column > maxColumn && canRewrite) reFormat(trimmed)
            else if (hasSpace) sb.append(trimmed)
            else {
              append(trimmed, 0, nonSlash).append(' ')
              append(trimmed, nonSlash, trimmed.length)
            }
          }
        }
        private def reFormat(text: String): Unit = {
          val useSlc = breakBefore && style.comments.wrapStandaloneSlcAsSlc
          val appendLineBreak: () => Unit =
            if (useSlc) {
              val spaces: String = getIndentation(indent)
              () => sb.append('\n').append(spaces).append("//")
            } else {
              val spaces: String = getIndentation(indent + 1)
              () => sb.append('\n').append(spaces).append('*')
            }
          val contents = text.substring(2).trim
          val wordIter = splitAsIterator(slcDelim)(contents)
          sb.append(if (useSlc) "//" else "/*")
          val curlen = sb.length
          val wf = new WordFormatter(appendLineBreak)
          val lines = wf(wordIter, getFirstLineLength, breakBefore)
          if (!useSlc) terminateMlc(curlen, lines)
        }
      }

      private class FormatMlc(text: String)(implicit sb: StringBuilder)
          extends FormatCommentBase(style.maxColumn) {
        private val spaces: String = getIndentation(indent + 1)

        def format(): Unit =
          // don't rewrite comments which contain nested comments
          if (canRewrite && text.lastIndexOf("/*") == 0) {
            val sectionIter = new SectIter {
              private val lineIter = {
                val header = mlcHeader.matcher(text)
                val beg = if (header.lookingAt()) header.end() else 2
                val contents = text.substring(beg, text.length - 2)
                splitAsIterator(mlcLineDelim)(contents).buffered
              }
              private def paraEnds: Boolean = lineIter.head.isEmpty

              override def hasNext = lineIter.hasNext
              override def next() = new ParaIter

              class ParaIter extends AbstractIterator[WordIter] {
                private var hasPara: Boolean = true
                override def hasNext: Boolean = hasPara && lineIter.hasNext && {
                  hasPara = !paraEnds
                  if (!hasPara)
                    do lineIter.next() while (lineIter.hasNext && paraEnds)
                  hasPara
                }
                override def next() = new ParaLineIter()
                  .flatMap(splitAsIterator(slcDelim))

                class ParaLineIter extends AbstractIterator[String] {
                  private var hasLine: Boolean = true
                  override def hasNext: Boolean = hasLine
                  override def next(): String = {
                    val head = lineIter.next()
                    hasLine = lineIter.hasNext && !paraEnds &&
                      !mlcParagraphEnd.matcher(head).find() &&
                      !mlcParagraphBeg.matcher(lineIter.head).find()
                    head
                  }
                }
              }
            }
            sb.append("/*")
            val curlen = sb.length
            val lines = iterSections(sectionIter)
            terminateMlc(curlen, lines)
          } else {
            val trimmed = removeTrailingWhiteSpace(text)
            sb.append(leadingAsteriskSpace.matcher(trimmed).replaceAll(spaces))
          }

        private def appendLineBreak(): Unit = sb.append('\n').append(spaces)
          .append('*')

        private val wf = new WordFormatter(appendLineBreak)

        private type ParaIter = Iterator[WordIter]
        private def iterParagraphs(
            iter: ParaIter,
            firstLineLen: Int,
            atLineBeg: Boolean,
        ): Int = {
          var lines = wf(iter.next(), firstLineLen, atLineBeg)
          while (iter.hasNext) {
            appendLineBreak()
            lines += 1 + wf(iter.next(), leadingMargin, true)
          }
          lines
        }

        private type SectIter = Iterator[ParaIter]
        private def iterSections(iter: SectIter): Int = {
          var lines =
            iterParagraphs(iter.next(), getFirstLineLength, breakBefore)
          while (iter.hasNext) {
            appendLineBreak()
            appendLineBreak()
            lines += 2 + iterParagraphs(iter.next(), leadingMargin, true)
          }
          lines
        }
      }

      private class FormatMlDoc(isWrap: Boolean)(text: String)(implicit
          sb: StringBuilder,
      ) extends FormatCommentBase(
            if (isWrap) style.docstringsWrapMaxColumn else style.maxColumn,
            if (style.docstrings.style eq Docstrings.SpaceAsterisk) 2 else 1,
            if (style.docstrings.style eq Docstrings.AsteriskSpace) 1 else 0,
          ) {
        def this(text: String)(implicit sb: StringBuilder) = this(
          (style.docstrings.wrap eq Docstrings.Wrap.yes) && curr.isStandalone,
        )(text)

        private val spaces: String = getIndentation(indent + extraIndent)
        private val margin = getIndentation(1 + leadingMargin)

        def format(): Unit = {
          val docOpt =
            if (isWrap) ScaladocParser.parse(tok.meta.left.text) else None
          docOpt.fold(formatNoWrap())(formatWithWrap)
        }

        private def formatWithWrap(doc: Scaladoc): Unit = {
          sb.append("/**")
          val sbLen =
            if (style.docstrings.skipFirstLineIf(false)) {
              appendBreak()
              0 // force margin but not extra asterisk
            } else {
              sb.append(' ')
              sb.length
            }
          val paras = doc.para.iterator
          paras.foreach { para =>
            para.terms.foreach {
              formatTerm(_, margin, sbNonEmpty = sb.length != sbLen)
            }
            if (paras.hasNext) appendBreak()
          }
          if (sb.length == sbLen) sb.append('*')
          sb.append('/')
        }

        private def formatTerm(
            term: Scaladoc.Term,
            termIndent: String,
            sbNonEmpty: Boolean,
        ): Unit = {
          def forceFirstLine(): Unit = {
            // don't output on top line
            // lists/fenced code blocks are sensitive to margin
            sb.setLength(sb.length - 1) // remove space
            appendBreak()
          }
          if (sbNonEmpty) sb.append(termIndent)
          term match {
            case t: Scaladoc.CodeBlock =>
              sb.append("{{{")
              val nested = t.code.headOption.exists(_.endsWith("// scala"))
              formatCodeBlock(nested, t.code, margin, isRelative = false)
              sb.append(termIndent).append("}}}")
              appendBreak()
            case t: Scaladoc.MdCodeBlock =>
              // first spaces (after asterisk) on all lines must align
              if (!sbNonEmpty && leadingMargin != 0) {
                forceFirstLine()
                sb.append(termIndent)
              }
              sb.append(t.fence)
              if (t.info.nonEmpty) {
                sb.append(t.info.head)
                t.info.tail.foreach(x => sb.append(' ').append(x))
              }
              val nested = t.info.headOption.contains("scala")
              formatCodeBlock(nested, t.code, termIndent, isRelative = true)
              sb.append(termIndent).append(t.fence)
              appendBreak()
            case t: Scaladoc.Heading =>
              val delimiter = "=" * t.level
              sb.append(delimiter).append(t.title).append(delimiter)
              appendBreak()
            case t: Scaladoc.Tag =>
              sb.append(t.tag.tag)
              t.label.foreach(x => sb.append(' ').append(x.syntax))
              appendBreak()
              if (t.desc.nonEmpty) {
                val tagIndent = getIndentation(2 + termIndent.length)
                t.desc.foreach(formatTerm(_, tagIndent, sbNonEmpty = true))
              }
            case t: Scaladoc.ListBlock =>
              // outputs margin space and appends new line, too
              // therefore, let's start by "rewinding"
              if (sbNonEmpty || leadingMargin == 0) sb
                .setLength(sb.length - termIndent.length)
              else forceFirstLine()
              val listIndent = // shift initial only by 2
                if (termIndent ne margin) termIndent
                else getIndentation(margin.length + 2)
              formatListBlock(listIndent)(t)
            case t: Scaladoc.Text => formatTextAfterMargin(t, termIndent)
            case t: Scaladoc.Table => formatTable(t, termIndent)
          }
        }

        private def formatTextAfterMargin(
            text: Scaladoc.Text,
            termIndent: String,
        ): Unit = {
          def prefixFirstWord(word: String): String = {
            def likeNonText = word.startsWith("```") ||
              word.startsWith("~~~") || // code fence
              word.startsWith("@") || // tag
              word.startsWith("=") || // heading
              word.startsWith("|") || word.startsWith("+-") || // table
              word == "-" || // list, this and next
              word.length == 2 && word(1) == '.' && "1aiI".contains(word(0))
            if (likeNonText) "\\" else "" // escape if parser can be confused
          }

          val wf = new WordFormatter(appendBreak, termIndent, prefixFirstWord)
          val words = text.parts.iterator.map(_.syntax)
          wf(words, termIndent.length, true, false)
          appendBreak()
        }

        private def formatCodeBlock(
            nested: Boolean,
            code: Seq[String],
            termIndent: String,
            isRelative: Boolean,
        ): Unit = {
          val ok = nested && formatScalaCodeBlock(code, termIndent)
          if (!ok) formatCodeBlock(code, termIndent, isRelative)
        }

        private def formatCodeBlock(
            code: Seq[String],
            termIndent: String,
            isRelative: Boolean,
        ): Unit = {
          val offsetOpt =
            if (isRelative) None
            else {
              val minSpaces = code.foldLeft(Int.MaxValue) { (res, x) =>
                val matcher = docstringLeadingSpace.matcher(x)
                if (matcher.lookingAt()) math.min(res, matcher.end())
                else if (x.nonEmpty) 0
                else res
              }
              if (minSpaces < Int.MaxValue) {
                val offset = minSpaces - termIndent.length()
                val spaces = if (offset > 0) getIndentation(offset) else ""
                Some((spaces, minSpaces))
              } else None
            }

          appendBreak()
          code.foreach { x =>
            if (x.nonEmpty) {
              sb.append(termIndent)
              offsetOpt match {
                case Some((offset, lineStart)) =>
                  sb.append(offset)
                  append(x, lineStart, x.length)
                case _ => sb.append(x)
              }
            }
            appendBreak()
          }
        }

        private def formatScalaCodeBlock(
            code: Seq[String],
            termIndent: String,
        ): Boolean = {
          val codeStyle = style.copy(
            runner = style.runner.forCodeBlock,
            // let's not wrap docstrings, to avoid recursion
            docstrings = style.docstrings.withoutRewrites,
            maxColumn = style.maxColumn - spaces.length - termIndent.length - 1,
          )
          Scalafmt.format(code.mkString("\n"), codeStyle) match {
            case Formatted.Success(res) =>
              val formattedCode = res.split('\n')
              formatCodeBlock(formattedCode, termIndent, isRelative = true)
              true
            case _ => false
          }
        }

        private def formatListBlock(
            listIndent: String,
        )(block: Scaladoc.ListBlock): Unit = {
          val prefix = block.prefix
          val itemIndent = getIndentation(listIndent.length + prefix.length + 1)
          block.items.foreach { x =>
            sb.append(listIndent).append(prefix).append(' ')
            formatListTerm(itemIndent)(x)
          }
        }

        private def formatListTerm(
            itemIndent: String,
        )(item: Scaladoc.ListItem): Unit = {
          formatTextAfterMargin(item.text, itemIndent)
          item.terms.foreach(formatTerm(_, itemIndent, sbNonEmpty = true))
        }

        private def formatTable(table: Scaladoc.Table, termIndent: String): Unit = {
          val align = table.align
          def getRowMax(f: Scaladoc.Table.Row => Int): Int = table.rows
            .foldLeft(f(table.header)) { case (out, row) =>
              math.max(out, f(row))
            }
          val colsRange = 0 until getRowMax(_.cols.length)
          val maxLengths = colsRange.map { x =>
            getRowMax(_.cols.view.drop(x).headOption.fold(0)(_.length))
          }

          @inline
          def beforeAll(): Unit = sb.append(termIndent)
          @inline
          def beforeEach(): Unit = sb.append('|')
          @inline
          def afterAll(): Unit = { sb.append('|'); appendBreak() }
          @inline
          def getAlign(col: Int) =
            if (col < align.length) align(col) else Scaladoc.Table.Left
          def formatCols(useMargin: Boolean)(f: Int => Unit): Unit = {
            if (useMargin) beforeAll()
            colsRange.foreach { x => beforeEach(); f(x) }
            afterAll()
          }

          def formatRow(useMargin: Boolean)(row: Scaladoc.Table.Row): Unit =
            formatCols(useMargin) { col =>
              val cell = if (col < row.cols.length) row.cols(col) else ""
              val pad = maxLengths(col) - cell.length
              val lpad = getAlign(col).leftPad(pad)
              sb.append(getIndentation(1 + lpad)).append(cell)
                .append(getIndentation(1 + pad - lpad))
            }
          formatRow(false)(table.header)
          formatCols(true) { col =>
            sb.append(getAlign(col).syntax(maxLengths(col)))
          }
          table.rows.foreach(formatRow(true))
        }

        private def formatNoWrap(): Unit = {
          // remove "/*" (keep one asterisk) and "*/"
          val trimmed = CharBuffer.wrap(text, 2, text.length - 2)
          val matcher = docstringLine.matcher(trimmed)
          sb.append("/**")
          val sbLen = sb.length
          @tailrec
          def iter(prevWasBlank: Boolean): Unit = if (matcher.find()) {
            val contentBeg = matcher.start(2)
            val contentEnd = matcher.end(2)
            if (contentBeg == contentEnd) iter(true)
            else {
              if (sb.length != sbLen) {
                if (prevWasBlank) appendBreak()
                appendBreak().append(margin)
              } else if (style.docstrings.skipFirstLineIf(prevWasBlank))
                appendBreak().append(margin)
              else sb.append(' ')
              val extraMargin = matcher.end(1) - matcher.start(1) -
                margin.length
              if (extraMargin > 0) sb.append(getIndentation(extraMargin))
              append(trimmed, contentBeg, contentEnd)
              iter(false)
            }
          }
          iter(false)
          appendBreak().append('/')
        }

        @inline
        private def appendBreak() = sb.append('\n').append(spaces).append('*')
      }

    }

    /** Returns how many extra spaces are needed to align tokens, as configured
      * by `initStyle.align.tokens`.
      */
    // TODO(olafur) Refactor implementation to make it maintainable. It's super
    // imperative and error-prone right now.
    private def alignmentTokens: Map[Int, Int] = {
      lazy val noAlignTokens = styleMap.forall(_.align.tokens.isEmpty)
      if (locations.length != tokens.length || noAlignTokens) Map
        .empty[Int, Int]
      else {
        var columnShift = 0
        implicit val finalResult = Map.newBuilder[Int, Int]
        val isMultiline = styleMap.init.align.multiline

        // all blocks must be here, to get final flush
        val blocks = new mutable.HashMap[Tree, AlignBlock]
        def createBlock(x: Tree) = blocks.getOrElseUpdate(x, new AlignBlock)
        var prevAlignContainer: Tree = null
        var prevBlock: AlignBlock = if (isMultiline) null else createBlock(null)
        val getOrCreateBlock: Tree => AlignBlock =
          if (isMultiline) createBlock else _ => prevBlock
        val getBlockToFlush: (=> Tree, Boolean) => Option[AlignBlock] =
          if (isMultiline) // don't flush unless blank line
            (x, isBlankLine) => if (isBlankLine) blocks.get(x) else None
          else (_, _) => Some(prevBlock)
        val shouldFlush: Tree => Boolean =
          if (!isMultiline) _ => true else (x: Tree) => x eq prevAlignContainer
        val wasSameContainer: Tree => Boolean =
          if (isMultiline) _ => true else (x: Tree) => x eq prevAlignContainer

        var idx = 0
        while (idx < locations.length) {
          var alignContainer: Tree = null
          val columnCandidates = IndexedSeq.newBuilder[AlignStop]
          @tailrec
          def processLine: FormatLocation = {
            if (idx > 0) {
              val prevFloc = locations(idx - 1)
              if (prevFloc.hasBreakAfter || prevFloc.formatToken.leftHasNewline)
                columnShift = 0
            }
            implicit val floc: FormatLocation = locations(idx)
            val ft = floc.formatToken
            idx += 1
            columnShift += floc.shift
            if (floc.hasBreakAfter || ft.leftHasNewline) floc
            else {
              getAlignNonSlcOwner(ft).foreach { nonSlcOwner =>
                val container =
                  getAlignContainer(nonSlcOwner.getOrElse(ft.meta.rightOwner))
                def appendCandidate() = columnCandidates += new AlignStop(
                  getAlignColumn(floc) + columnShift,
                  floc,
                  getAlignHashKey(floc),
                  nonSlcOwner,
                )
                if (alignContainer eq null) alignContainer = container
                else if (alignContainer ne container) {
                  val pos1 = alignContainer.pos
                  if (nonSlcOwner.isEmpty) {
                    val prevFt = tokens.prevNonCommentSameLine(ft)
                    if (pos1.end >= prevFt.left.end) appendCandidate()
                  } else {
                    val pos2 = container.pos
                    if (pos2.start <= pos1.start && pos2.end >= pos1.end) {
                      alignContainer = container
                      columnCandidates.clear()
                    }
                  }
                }
                if (alignContainer eq container) appendCandidate()
              }
              if (idx < locations.length) processLine else floc
            }
          }

          implicit val floc: FormatLocation = processLine
          val isBlankLine = floc.state.split.modExt.mod.newlines > 1
          if (alignContainer ne null) {
            val candidates = columnCandidates.result()
            val block = getOrCreateBlock(alignContainer)
            val blockWasEmpty = block.isEmpty
            if (!blockWasEmpty || !isBlankLine) {
              val alignLine = new AlignLine(
                candidates,
                floc.state.prev.column + columnShift,
                floc.style,
              )
              val appendToEmptyBlock = blockWasEmpty || {
                val sameOwner = wasSameContainer(alignContainer)
                val matches = columnMatches(block, alignLine, sameOwner)

                val flush =
                  if (matches == 0) shouldFlush(alignContainer)
                  else !block.tryAppendToBlock(alignLine, matches)

                (isBlankLine || flush) && {
                  flushAlignBlock(block)
                  !isBlankLine
                }
              }

              if (appendToEmptyBlock) block.appendToEmptyBlock(alignLine)
            }

            prevAlignContainer = alignContainer
            prevBlock = block
          }
          if (isBlankLine || alignContainer.eq(null)) getBlockToFlush(
            getAlignContainer(floc.formatToken.meta.rightOwner),
            isBlankLine,
          ).foreach(flushAlignBlock)
        }
        blocks.valuesIterator.foreach(flushAlignBlock)
        finalResult.result()
      }
    }

    private def isEarlierLine(t: Tree)(implicit fl: FormatLocation): Boolean = {
      val idx = tokens.getHead(t).meta.idx + 1
      idx <= fl.formatToken.meta.idx && // e.g., leading comments
      locations(idx).leftLineId != fl.leftLineId
    }

    private def onSingleLine(t: Tree): Boolean = {
      val ttokens = t.tokens
      val beg = tokens.after(ttokens.head)
      val end = tokens.before(ttokens.last)
      getLineDiff(locations, beg, end) == 0
    }

    object AlignContainer {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case _: Source | _: Template | _: Term.Block | _: Term.Match |
            _: Type.Match | _: Term.FunctionTerm | _: Term.PartialFunction =>
          Some(tree)
        case _ => None
      }

      object WithBody {
        def unapply(tree: Tree): Option[(List[meta.Mod], Tree)] = tree match {
          case wm: Stat.WithMods => tree match {
              case t: Tree.WithBody => Some(wm.mods -> t.body)
              case t: Stat.WithTemplate => Some(wm.mods -> t.templ)
              case _ => None
            }
          case _ => None
        }
      }
    }

    @tailrec
    private def getAlignContainerParent(
        child: Tree,
        maybeParent: Option[Tree] = None,
    )(implicit fl: FormatLocation): Tree =
      maybeParent.orElse(child.parent) match {
        case Some(AlignContainer(p)) => p
        case Some(p @ (_: Term.Select | _: Pat.Var | _: Term.ApplyInfix)) =>
          getAlignContainerParent(p)
        case Some(p: Term.Apply) if (p.argClause.values match {
              case (_: Term.Apply) :: Nil => true
              case _ => p.fun eq child
            }) => getAlignContainerParent(p)
        // containers that can be traversed further if on same line
        case Some(p @ (_: Case | _: Enumerator)) =>
          if (isEarlierLine(p)) p else getAlignContainerParent(p)
        // containers that can be traversed further if lhs single-line
        case Some(p @ AlignContainer.WithBody(mods, b)) =>
          val keepGoing = {
            val ptokens = p.tokens
            val beg = mods.lastOption.fold(tokens.after(ptokens.head)) { m =>
              tokens.next(tokens.tokenAfter(m))
            }
            val useBody = b.eq(child) || p.eq(child)
            val beforeBody = if (useBody) tokens.tokenJustBeforeOpt(b) else None
            val end = beforeBody.getOrElse(tokens.before(ptokens.last))
            getLineDiff(locations, beg, end) == 0
          }
          if (keepGoing) getAlignContainerParent(p) else p
        case Some(p: Term.ForYield) if child ne p.body => p
        case Some(p: Member.ParamClause) => p.parent match {
            // if all on single line, keep going
            case Some(q) if onSingleLine(q) => getAlignContainerParent(p)
            // if this one not on single line, use parent as the owner
            case Some(q) if !onSingleLine(p) => // skip ParamClauseGroup
              if (q.is[Member.ParamClauseGroup]) q.parent.getOrElse(q) else q
            case _ => p // this one on single line, but the rest are not
          }
        case Some(p: Member.SyntaxValuesClause) => getAlignContainerParent(p)
        case Some(p: Member.ParamClauseGroup) => getAlignContainerParent(p)
        case Some(p) => p.parent.getOrElse(p)
        case _ => child
      }

    @tailrec
    private def getAlignContainer(t: Tree)(implicit fl: FormatLocation): Tree =
      t match {
        case AlignContainer(x) if fl.formatToken.right.is[T.Comment] => x

        case _: Defn | _: Case | _: Term.Apply | _: Init | _: Ctor.Primary =>
          getAlignContainerParent(t, Some(t))

        case _: Mod => t.parent match {
            case Some(p) => getAlignContainer(p)
            case None => t
          }

        case _ => getAlignContainerParent(t)
      }

    private def flushAlignBlock(
        block: AlignBlock,
    )(implicit builder: mutable.Builder[(Int, Int), Map[Int, Int]]): Unit = {
      if (block.hasMultiple) flushMultiEntryAlignBlock(block)
      block.clear()
    }

    private def flushMultiEntryAlignBlock(
        block: AlignBlock,
    )(implicit builder: mutable.Builder[(Int, Int), Map[Int, Int]]): Unit = {
      val endIndex = locations.length - 1
      block.foreach { x =>
        if (x.style.align.multiline) {
          val headStop = x.stops.head
          val ftIndex = headStop.ft.meta.idx
          if (ftIndex < endIndex)
            shiftStateColumnIndent(ftIndex + 1, headStop.shift)
        }
        var previousShift = 0
        x.stops.foreach { stop =>
          if (stop.isActive) {
            val currentShift = stop.shift
            val offset = currentShift - previousShift
            if (offset > 0) {
              builder += stop.ft.meta.idx -> offset
              previousShift = currentShift
            }
          }
        }
      }
    }

    private def shiftStateColumnIndent(startIdx: Int, offset: Int): Unit = {
      // look for StateColumn; it returns indent=0 for withStateOffset(0)
      val stateIndentOpt = locations(startIdx).state.split.modExt.indents
        .filter(_.hasStateColumn).flatMap(_.withStateOffset(0))
      stateIndentOpt.headOption.foreach { indent =>
        @tailrec
        def updateLocation(idx: Int): Unit = {
          val floc = locations(idx)
          if (indent.notExpiredBy(floc.formatToken)) {
            val state = floc.state
            if (state.split.isNL) locations(idx) = floc
              .copy(state = state.copy(indentation = state.indentation + offset))
            val nextIdx = idx + 1
            if (nextIdx < locations.length) updateLocation(nextIdx)
          }
        }
        updateLocation(startIdx)
      }
    }

    lazy val extraBlankTokens = {
      val extraBlankMap = new mutable.HashMap[Int, Int]
      def setIdx(idx: Int, cnt: Int) =
        if (extraBlankMap.getOrElseUpdate(idx, cnt) < cnt) extraBlankMap
          .update(idx, cnt)
      @inline
      def setIdxCheck(idx: => Int, cnt: Int, force: => Boolean) =
        if (cnt > 0) setIdx(idx, cnt) else if (cnt < 0 && force) setIdx(idx, 0)
      @inline
      def setFt(ft: FormatToken) = setIdx(ft.meta.idx, 1)
      @inline
      def setFtCheck(ft: FormatToken, cnt: Int, force: => Boolean) =
        setIdxCheck(ft.meta.idx, cnt, force)
      def setTopStats(owner: Tree, stats: Seq[Stat]): Unit = {
        val nest = getNest(stats.head)
        if (nest < 0) return
        val end = owner.pos.end
        val notUnindentedPkg = owner match {
          case t: Pkg => indentedPackage(t)
          case _ => true
        }
        def setStat(
            stat: Tree,
            idx: Int,
            isLast: Boolean,
        ): Option[(Int, Newlines.NumBlanks)] = setStats(idx, stat, stat, isLast)
        def blanks(cnt: => Int, edge: Boolean, edgeCnt: => Option[Int]): Int =
          if (edge) edgeCnt.getOrElse(math.min(cnt, 1)) else cnt
        @inline
        def blanksBefore(cnt: Newlines.NumBlanks, isFirst: Boolean): Int =
          blanks(cnt.before, isFirst, cnt.beforeAll)
        @inline
        def blanksAfter(cnt: Newlines.NumBlanks, isLast: Boolean): Int =
          blanks(cnt.after, isLast, cnt.afterAll)
        def setStats(
            idx: Int,
            stat: Tree,
            statLast: Tree,
            isLast: Boolean,
        ): Option[(Int, Newlines.NumBlanks)] = getBlanks(stat, statLast, nest)
          .map { case (x, head, last) =>
            val beforeCnt = blanksBefore(x, notUnindentedPkg && idx == 0)
            val beforeFt = leadingComment(head)
            setFtCheck(beforeFt, beforeCnt, head eq beforeFt)
            val afterFt = trailingComment(last, end)
            val lastIdx = afterFt.meta.idx
            val afterCnt = blanksAfter(x, isLast)
            setIdxCheck(lastIdx, afterCnt, last eq afterFt)
            (lastIdx, x)
          }
        def setEndMarker(
            stat: Term.EndMarker,
            prevIdx: Int,
            prevBlanks: Newlines.NumBlanks,
            isLast: Boolean,
        ): Unit = {
          val last = tokens.getLast(stat)
          if (prevBlanks.beforeEndMarker <= 0) extraBlankMap.remove(prevIdx)
          else extraBlankMap.update(prevIdx, prevBlanks.beforeEndMarker)
          val cnt = blanksAfter(prevBlanks, isLast)
          val afterFt = trailingComment(last, end)
          setFtCheck(afterFt, cnt, afterFt eq last)
        }
        @tailrec
        def iter(
            rest: Seq[Stat],
            idx: Int,
            previous: Option[(Int, Newlines.NumBlanks)],
            imports: Option[(Int, ImportExportStat, ImportExportStat)],
        ): Unit = {
          val stat = rest.head
          val newRest = rest.tail
          val isLast = newRest.isEmpty
          val ok = stat match {
            case t: Term.EndMarker =>
              previous.foreach { case (prevIdx, prevBlanks) =>
                setEndMarker(t, prevIdx, prevBlanks, isLast)
              }
              false
            case t: Pkg => indentedPackage(t)
            case _: ImportExportStat => false
            case _ => true
          }
          val newImports = stat match {
            case t: ImportExportStat =>
              val (idxHead, head) = imports.fold((idx, t)) { case (i, h, _) =>
                (i, h)
              }
              if (!isLast) Some((idxHead, head, t))
              else {
                setStats(idxHead, head, t, true)
                None
              }
            case _ =>
              imports.foreach { case (idxHead, head, last) =>
                setStats(idxHead, head, last, false)
              }
              None
          }
          val newPrevious = if (ok) setStat(stat, idx, isLast) else None
          if (!isLast) iter(newRest, idx + 1, newPrevious, newImports)
        }
        iter(stats, 0, None, None)
      }
      def insideBody(stats: Seq[Stat], nl: Newlines, ba: Newlines.BeforeAfter) =
        stats.lengthCompare(nl.topLevelBodyMinStatements) >= 0 &&
          nl.topLevelBodyIfMinStatements.contains(ba)
      def beforeBody(stats: Seq[Stat])(altOk: Newlines => Boolean): Unit = {
        val ft = tokens.tokenJustBefore(stats.head)
        val nl = locations(ft.meta.idx).style.newlines
        val ok = insideBody(stats, nl, Newlines.before) || altOk(nl)
        if (ok) setFt(leadingComment(ft))
      }
      def afterBody(owner: Tree, stats: Seq[Stat]): Unit = {
        val ft = tokens.getLast(stats.last)
        val nl = locations(ft.meta.idx).style.newlines
        if (insideBody(stats, nl, Newlines.after))
          setFt(trailingComment(ft, owner.pos.end))
      }
      val trav = new Traverser {
        override def apply(tree: Tree): Unit = tree match {
          case t: Source =>
            val stats = t.stats
            if (stats.nonEmpty) {
              setTopStats(t, stats)
              super.apply(stats)
            }
          case t: Template =>
            val stats = t.stats
            if (stats.nonEmpty) {
              beforeBody(stats) {
                _.beforeTemplateBodyIfBreakInParentCtors && {
                  val beg = leadingComment(t).meta.idx
                  val end = tokens(templateCurlyOrLastNonTrivial(t)).meta.idx
                  locations(beg).leftLineId != locations(end).leftLineId
                }
              }
              afterBody(t, stats)
              setTopStats(t, stats)
              super.apply(stats) // skip inits
            }
          case t: Defn.ExtensionGroup =>
            val stats = t.body match {
              case b: Term.Block => b.stats
              case b => List(b)
            }
            if (stats.nonEmpty) {
              beforeBody(stats)(_ => false)
              afterBody(t, stats)
              setTopStats(t, stats)
              super.apply(stats)
            }
          case t: Pkg =>
            val stats = t.stats
            if (stats.nonEmpty) {
              if (indentedPackage(t)) {
                beforeBody(stats)(_ => false)
                afterBody(t, stats)
              } else if (
                stats.head match {
                  case pkg: Pkg => indentedPackage(pkg)
                  case _ => true
                }
              ) beforeBody(stats)(_.hasTopStatBlankLines)
              setTopStats(t, stats)
              super.apply(stats) // skip ref
            }
          case t: Stat.WithTemplate => apply(t.templ)
          case _ => // everything else is not "top-level"
        }
      }

      if (locations.length == tokens.length) trav(topSourceTree)
      extraBlankMap.toMap
    }

    def indentedPackage(pkg: Pkg) = tokens.tokenAfter(pkg.ref).right match {
      case _: T.LeftBrace | _: T.Colon => true
      case _ => false
    }

    @tailrec
    final def getNest(tree: Tree, curNest: Int = 0): Int = tree.parent match {
      case Some(_: Source) | None => curNest
      case Some(t: Template) => getNest(t, curNest)
      case Some(t: Pkg) if !indentedPackage(t) => getNest(t, curNest)
      case Some(t) => getNest(t, curNest + 1)
    }

    def getBlanks(
        statHead: Tree,
        statLast: Tree,
        nest: Int,
    ): Option[(Newlines.NumBlanks, FormatToken, FormatToken)] = {
      val head = tokens.tokenJustBefore(statHead)
      val last = tokens.getLast(statLast)
      val bLoc = locations(head.meta.idx + 1)
      val numBreaks = getLineDiff(bLoc, locations(last.meta.idx))
      bLoc.style.newlines.getTopStatBlankLines(statHead, numBreaks, nest).map {
        x => (x, head, last)
      }
    }

  }

  private def getAlignHashKey(location: FormatLocation): Int = {
    val ft = location.formatToken
    val align = location.style.align
    val ownerKey = {
      val treeKind = ft.meta.rightOwner.productPrefix
      align.treeCategory.getOrElse(treeKind, treeKind)
    }
    val tokenKey = {
      val syntax = ft.right.productPrefix
      align.tokenCategory.getOrElse(syntax, syntax)
    }
    (tokenKey, ownerKey).hashCode()
  }

  private def columnMatches(
      block: AlignBlock,
      line: AlignLine,
      sameOwner: Boolean,
  )(implicit floc: FormatLocation): Int = {
    val checkEol: (Tree => Boolean) => Boolean =
      if (floc.style.align.multiline) _ => true
      else {
        val endOfLineOwner = floc.formatToken.meta.rightOwner
        TreeOps.findTreeWithParentSimple(endOfLineOwner)(_).isEmpty
      }
    @tailrec
    def iter(pairs: Seq[(AlignStop, AlignStop)], cnt: Int): Int = pairs match {
      case (r1, r2) +: tail =>
        // skip checking if row1 and row2 matches if both of them continues to a single line of comment
        // in order to vertical align adjacent single lines of comment.
        // see: https://github.com/scalameta/scalafmt/issues/1242
        val ok = (r1.nonSlcOwner, r2.nonSlcOwner) match {
          case (Some(row1Owner), Some(row2Owner)) =>
            def isRowOwner(x: Tree) = (x eq row1Owner) || (x eq row2Owner)
            sameOwner && r1.hashKey == r2.hashKey && checkEol(isRowOwner)
          case (x1, x2) => x1 eq x2
        }
        if (ok) iter(tail, cnt + 1) else cnt
      case _ => cnt
    }
    iter(block.refStops.zip(line.stops), 0)
  }

}

object FormatWriter {

  private val NoLine = Int.MaxValue

  case class FormatLocation(
      formatToken: FormatToken,
      state: State,
      style: ScalafmtConfig,
      leftLineId: Int, // counts back from the end of the file
      shift: Int = 0,
      optionalBraces: Map[Int, Tree] = Map.empty,
      // if indent is empty, indicates open; otherwise, whether to tuck
      missingBracesOpenOrTuck: Boolean = false,
      missingBracesIndent: Set[Int] = Set.empty,
      replace: String = null,
  ) {
    def hasBreakAfter: Boolean = state.split.isNL
    def hasBreakBefore: Boolean =
      // first token is BOF
      formatToken.meta.idx <= 1 || state.prev.split.isNL
    def isStandalone: Boolean = hasBreakAfter && hasBreakBefore
    @inline
    def isNotRemoved: Boolean = leftLineId != NoLine
    @inline
    def remove: FormatLocation = copy(leftLineId = NoLine)
  }

  class AlignStopColumn(var column: Int = -1) {
    @inline
    def reset(): Unit = column = -1
  }
  class AlignStop(
      val column: Int,
      val floc: FormatLocation,
      val hashKey: Int,
      val nonSlcOwner: Option[Tree],
  ) {
    var shiftedColumn: AlignStopColumn = new AlignStopColumn
    @inline
    def ft = floc.formatToken
    @inline
    def shifted = shiftedColumn.column
    @inline
    def shifted_=(value: Int) = shiftedColumn.column = value
    @inline
    def isActive = shifted >= 0
    @inline
    def shift = shifted - column
  }

  class AlignLine(
      val stops: IndexedSeq[AlignStop],
      val eolColumn: Int,
      val style: ScalafmtConfig,
  ) {
    @inline
    def noOverflow(shift: Int) = eolColumn + shift <= style.maxColumn
  }

  class AlignBlock(
      buffer: mutable.ArrayBuffer[AlignLine] =
        new mutable.ArrayBuffer[AlignLine],
      var refStops: IndexedSeq[AlignStop] = IndexedSeq.empty,
  ) {
    def appendToEmptyBlock(line: AlignLine): Unit = {
      refStops = line.stops
      buffer += line
      refStops.foreach(s => s.shifted = s.column)
    }

    def tryAppendToBlock(line: AlignLine, matches: Int): Boolean = {
      // truncate if matches are shorter than both lists
      val truncate = shouldTruncate(line, matches)
      val curStops = line.stops
      val refLen = if (truncate < 0) matches else refStops.length
      val curLen = if (truncate > 0) matches else curStops.length
      val newStopLen = refLen.max(curLen)

      // compute new stops for the block
      var refShift = 0
      var curShift = 0
      val newColumns = new mutable.ArrayBuffer[Int](newStopLen)
      val newStops = new mutable.ArrayBuffer[AlignStop](newStopLen)

      @tailrec
      def iter(refIdx: Int, curIdx: Int, refOk: Boolean = true): Boolean = {
        val refStop = if (refOk && refIdx < refLen) refStops(refIdx) else null
        val curStop = if (curIdx < curLen) curStops(curIdx) else null

        @inline
        def shiftRefColumn() = refShift + refStop.shifted
        @inline
        def shiftCurColumn() = curShift + curStop.column
        def retainRefStop(): Unit = {
          newStops += refStop
          newColumns += shiftRefColumn()
        }
        def appendCurStop(): Unit = {
          newStops += curStop
          newColumns += shiftCurColumn()
        }
        def updateStop(): Unit = {
          val refColumn = shiftRefColumn()
          val curColumn = shiftCurColumn()
          val diff = curColumn - refColumn
          if (diff > 0) {
            refShift += diff
            newColumns += curColumn
          } else {
            curShift -= diff
            newColumns += refColumn
          }
          curStop.shiftedColumn = refStop.shiftedColumn
          newStops += curStop
        }
        @inline
        def endRef() = Some((refIdx, curIdx + 1, false))
        @inline
        def endCur() = Some((refIdx + 1, curLen, true))

        def matchStops() = {
          updateStop()
          Some((refIdx + 1, curIdx + 1, true))
        }

        {
          if (refStop == null && curStop == null) // reached end together
            None
          else if (refStop == null) { // refStops are shorter
            appendCurStop()
            endRef()
          } else if (curStop == null) { // curStops are shorter
            retainRefStop()
            endCur()
          } else matchStops()
        } match {
          case Some((ridx, cidx, rok)) => iter(ridx, cidx, rok)
          case None => finalize(if (refOk) refIdx + 1 else refIdx)
        }
      }

      def finalize(endRefIdx: Int): Boolean = newStops.nonEmpty &&
        (line.style.align.allowOverflow || // check overflow
          line.noOverflow(curShift) && buffer.forall { bl =>
            bl.style.align.allowOverflow ||
            bl.stops.reverseIterator.find(_.isActive).forall { bs =>
              val shiftedColumn = bs.shifted
              val idx = newStops.lastIndexWhere { ns =>
                ns.isActive && ns.shifted <= shiftedColumn
              }
              idx >= 0 && bl.noOverflow(newColumns(idx) - bs.column)
            }
          }) && {
          // now we mutate
          (0 until newStops.length)
            .foreach(idx => newStops(idx).shifted = newColumns(idx))
          (endRefIdx until refStops.length)
            .foreach(refStops(_).shiftedColumn.reset())

          buffer += line
          refStops = newStops.toIndexedSeq
          true
        }

      iter(0, 0)
    }

    // <0 old, 0 neither, >0 new
    private def shouldTruncate(line: AlignLine, matches: Int): Int = {
      // truncate if matches are shorter than both lists
      val oldStops = refStops.length
      val newStops = line.stops.length
      if (matches == 0 || matches >= oldStops || matches >= newStops) 0
      else if (oldStops < newStops) -1 // new is longer
      else if (oldStops > newStops || hasMultiple) 1 // old is longer
      else if (line.stops.last.ft.right.is[T.Comment]) 1
      else -1
    }

    def clear(): Unit = {
      buffer.clear()
      refStops = IndexedSeq.empty
    }

    @inline
    def isEmpty: Boolean = buffer.isEmpty
    @inline
    def hasMultiple: Boolean = buffer.lengthCompare(1) > 0
    @inline
    def foreach[A](f: AlignLine => A): Unit = buffer.foreach(f)
  }

  /** Separator length gap needed to align blocks with different token lengths
    * by expression names, not tokens themselves.
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
    */
  def getAlignColumn(floc: FormatLocation): Int = {
    // if we didn't care about align token lengths, we'd always "useLeft"
    val useLeft = floc.formatToken.right.is[T.Comment]
    if (useLeft) floc.state.prev.column else floc.state.column
  }

  private def getAlignNonSlcOwner(
      ft: FormatToken,
  )(implicit floc: FormatLocation, tokens: FormatTokens): Option[Option[Tree]] = {
    def getNonSlcOwner = ft.meta.rightOwner match {
      case name: Term.Name => name.parent match {
          case Some(p: Term.ApplyInfix) => p
          case _ => name
        }
      case x => x
    }

    val slc = tokens.isRightLikeSingleLineComment(ft)
    val code = if (slc) "//" else ft.meta.right.text
    floc.style.alignMap.get(code).flatMap { matchers =>
      // Corner case when line ends with comment
      val nonSlcOwner = if (slc) None else Some(getNonSlcOwner)
      val owner = nonSlcOwner.getOrElse(ft.meta.leftOwner)
      val ok = matchers.isEmpty || matchers.exists(_.matches(owner))
      if (ok) Some(nonSlcOwner) else None
    }
  }

  // cache indentations to some level
  private val indentations: IndexedSeq[String] = {
    val size = 64
    val buf = new mutable.ArrayBuffer[String](size)
    buf += ""
    // use the previous indentation to add another space
    (1 until size).foreach(_ => buf += " " + buf.last)
    buf.toIndexedSeq
  }

  // see if indentation level is cached first
  private def getIndentation(len: Int): String =
    if (len < indentations.length) indentations(len) else " " * len

  // cache newlines to some level
  private val extraNewlines: IndexedSeq[String] = {
    val size = 4
    val buf = new mutable.ArrayBuffer[String](size)
    buf += "\n"
    // use the previous indentation to add another newline
    (1 until size).foreach(_ => buf += "\n" + buf.last)
    buf.toIndexedSeq
  }

  // see if blank level is cached first
  private def getNewlines(extra: Int): String =
    if (extra < extraNewlines.length) extraNewlines(extra)
    else "\n" * (1 + extra)

  private val trailingSpace = Pattern.compile("\\h++$", Pattern.MULTILINE)
  private def removeTrailingWhiteSpace(str: String): String = trailingSpace
    .matcher(str).replaceAll("")

  private def splitAsIterator(regex: Pattern)(value: String): Iterator[String] =
    regex.splitAsStream(value).iterator().asScala

  // "slc" stands for single-line comment
  private val slcDelim = Pattern.compile("\\h++")
  // "mlc" stands for multi-line comment
  private val mlcHeader = Pattern.compile("^/\\*\\h*+(?:\n\\h*+[*]*+\\h*+)?")
  private val mlcLineDelim = Pattern.compile("\\h*+\n\\h*+[*]*+\\h*+")
  private val mlcParagraphEnd = Pattern.compile("[.:!?=]$")
  private val mlcParagraphBeg = Pattern.compile("^(?:[-*@=]|\\d++[.:])")

  private val leadingAsteriskSpace = Pattern.compile("(?<=\n)\\h*+(?=[*][^*])")
  private val docstringLine = Pattern
    .compile("^(?:\\h*+\\*)?(\\h*+)(.*?)\\h*+$", Pattern.MULTILINE)
  private val emptyLines = "\\h*+(\n\\h*+\\*?\\h*+)*"
  private val emptyDocstring = Pattern.compile(s"^/\\*\\*$emptyLines\\*/$$")
  private val onelineDocstring = {
    val oneline = "[^*\n\\h](?:[^\n]*[^\n\\h])?"
    Pattern.compile(s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/$$")
  }
  private val docstringLeadingSpace = Pattern.compile("^\\h++")

  @inline
  private def getStripMarginPattern(pipe: Char) =
    if (pipe == '|') leadingPipeSpace else compileStripMarginPattern(pipe)

  @inline
  private def compileStripMarginPattern(pipe: Char) = Pattern
    .compile(s"(?<=\n)\\h*+(?=\\$pipe)")

  private val leadingPipeSpace = compileStripMarginPattern('|')

  /** [[https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#the-end-marker]]
    */
  private def getEndMarkerLabel(tree: Tree): String = tree match {
    // templates
    case _: Term.NewAnonymous => "new"
    case t: Defn.Class => t.name.toString
    case t: Defn.Object => t.name.toString
    case t: Defn.Trait => t.name.toString
    case t: Defn.Enum => t.name.toString
    case t: Defn.Given =>
      val label = t.name.toString
      if (label.isEmpty) "given" else label
    case t: Pkg.Object => t.name.toString
    // definitions
    case t: Defn.Def => t.name.toString
    case t: Defn.Macro => t.name.toString
    case t: Defn.GivenAlias =>
      val label = t.name.toString
      if (label.isEmpty) "given" else label
    case t: Defn.Type => t.name.toString
    case t: Defn.Val => t.pats match {
        case List(Pat.Var(n)) => n.toString
        case _ => "val"
      }
    case t: Defn.Var => t.pats match {
        case List(Pat.Var(n)) => n.toString
        case _ => "var"
      }
    // other
    case t: Pkg => t.ref match {
        case x: Term.Name => x.toString
        case x: Term.Select => x.name.toString
        case _ => null
      }
    case _: Ctor.Secondary => "this"
    case _: Defn.ExtensionGroup => "extension"
    case _: Term.If => "if"
    case _: Term.While => "while"
    case _: Term.Match | _: Type.Match => "match"
    case _: Term.For | _: Term.ForYield => "for"
    case _: Term.Try | _: Term.TryWithHandler => "try"
    case _ => null
  }

  @inline
  private def getLineDiff(beg: FormatLocation, end: FormatLocation): Int =
    beg.leftLineId - end.leftLineId

  @inline
  private def getLineDiff(
      toks: Array[FormatLocation],
      beg: FormatToken,
      end: FormatToken,
  ): Int = getLineDiff(toks(beg.meta.idx), toks(end.meta.idx))

  def isEmptyDocstring(text: String): Boolean = emptyDocstring.matcher(text)
    .matches()

}
