package org.scalafmt.internal

import java.nio.CharBuffer
import java.util.regex.Pattern

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.config.{Comments, Docstrings, ScalafmtConfig}
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util.TokenOps._
import org.scalafmt.util.{LiteralOps, TreeOps}

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.mutable
import scala.meta.internal.Scaladoc
import scala.meta.internal.parsers.ScaladocParser
import scala.meta.tokens.Token
import scala.meta.tokens.{Token => T}
import scala.meta.transversers.Traverser
import scala.meta.{
  Case,
  Defn,
  Enumerator,
  Importer,
  Lit,
  Mod,
  Pat,
  Pkg,
  Source,
  Template,
  Term,
  Tree,
  Type
}

/**
  * Produces formatted output from sequence of splits.
  */
class FormatWriter(formatOps: FormatOps) {
  import FormatWriter._
  import formatOps._

  def mkString(state: State): String = {
    implicit val sb = new StringBuilder()
    val locations = getFormatLocations(state)

    locations.iterate.foreach { entry =>
      val location = entry.curr
      implicit val style: ScalafmtConfig = location.style
      val state = location.state.prev // this is the state for left
      val formatToken = location.formatToken

      formatToken.left match {
        // formatting flag fetches from the previous state because of
        // `formatToken.left` rendering. `FormatToken(x, // format: on)` will have
        // formatOff = false, but x still should not be formatted
        case _ if state.formatOff => sb.append(formatToken.meta.left.text)
        case _: T.Comment =>
          entry.formatComment
        case _: T.Interpolation.Part | _: T.Constant.String =>
          sb.append(entry.formatMarginized)
        case c: T.Constant.Int =>
          sb.append(LiteralOps.prettyPrintInteger(formatToken.meta.left.text))
        case c: T.Constant.Long =>
          sb.append(LiteralOps.prettyPrintInteger(formatToken.meta.left.text))
        case c: T.Constant.Float =>
          sb.append(LiteralOps.prettyPrintFloat(formatToken.meta.left.text))
        case c: T.Constant.Double =>
          sb.append(LiteralOps.prettyPrintDouble(formatToken.meta.left.text))
        case token =>
          val syntax =
            Option(location.replace).getOrElse(formatToken.meta.left.text)
          val rewrittenToken = style.rewriteTokens.getOrElse(syntax, syntax)
          sb.append(rewrittenToken)
      }

      entry.formatWhitespace
    }

    sb.toString()
  }

  def getFormatLocations(state: State): FormatLocations = {
    val toks = formatOps.tokens.arr
    require(toks.length >= state.depth, "splits !=")
    val result = new Array[FormatLocation](state.depth)

    @tailrec
    def iter(state: State, lineId: Int): Unit =
      if (state.depth != 0) {
        val prev = state.prev
        val idx = prev.depth
        val ft = toks(idx)
        val breaks = state.split.isNL || ft.leftHasNewline
        val newLineId = lineId + (if (breaks) 1 else 0)
        result(idx) = FormatLocation(ft, state, styleMap.at(ft), newLineId)
        iter(prev, newLineId)
      }
    iter(state, 0)

    if (
      initStyle.rewrite.rules.contains(RedundantBraces) &&
      !initStyle.rewrite.redundantBraces.parensForOneLineApply.contains(false)
    )
      replaceRedundantBraces(result)

    new FormatLocations(result)
  }

  private def replaceRedundantBraces(locations: Array[FormatLocation]): Unit = {
    // will map closing brace to opening brace and its line offset
    val lookup = mutable.Map.empty[Int, (Int, Int)]

    // iterate backwards, to encounter closing braces first
    var idx = locations.length - 1
    while (0 <= idx) {
      val loc = locations(idx)
      val tok = loc.formatToken
      val state = loc.state
      tok.left match {
        case rb: T.RightBrace => // look for "foo { bar }"
          tok.meta.leftOwner match {
            case b: Term.Block if b.parent.exists {
                  case ta: Term.Apply if ta.tokens.last eq rb =>
                    TreeOps.isSingleElement(ta.args, b)
                  case _ => false
                } && RedundantBraces.canRewriteWithParens(b) =>
              val beg = tokens(matching(rb))
              lookup.update(beg.meta.idx, tok.meta.idx -> loc.leftLineId)
            case _ =>
          }
        case _: T.LeftBrace =>
          lookup.remove(idx).foreach {
            case (end, endOffset) if endOffset == loc.leftLineId =>
              val inParentheses = loc.style.spaces.inParentheses
              // remove space before "{"
              val prevBegState =
                if (0 == idx || (state.prev.split.modExt.mod ne Space))
                  state.prev
                else {
                  val prevloc = locations(idx - 1)
                  val prevState =
                    state.prev.copy(split = state.prev.split.withMod(NoSplit))
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
                  val split = state.split.withMod(NoSplit)
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

    val tokenAligns: Map[Int, Int] = alignmentTokens

    def iterate: Iterator[Entry] =
      Iterator.range(0, locations.length).map(new Entry(_))

    private def getAlign(tok: FormatToken, alignOffset: Int = 0): Int =
      tokenAligns.get(tok.meta.idx).fold(0)(_ + alignOffset)

    class Entry(val i: Int) {
      val curr = locations(i)
      private implicit val style = curr.style
      def previous = locations(math.max(i - 1, 0))

      @inline def tok = curr.formatToken
      @inline def state = curr.state
      @inline def prevState = curr.state.prev
      @inline def lastModification = prevState.split.modExt.mod

      def getWhitespace(alignOffset: Int): String = {
        state.split.modExt.mod match {
          case Space =>
            val previousAlign =
              if (lastModification != NoSplit) 0
              else getAlign(previous.formatToken)
            val currentAlign = getAlign(tok, alignOffset)
            getIndentation(1 + currentAlign + previousAlign)

          case nl: NewlineT =>
            val isDouble = nl.isDouble ||
              style.newlines.forceBlankBeforeMultilineTopLevelStmt &&
                isMultilineTopLevelStatement(locations, i) ||
              style.newlines.forceBlankAfterMultilineTopLevelStmt &&
                locations.lengthCompare(i + 1) != 0 &&
                topLevelLastToHeadTokens.get(i).exists {
                  isMultilineTopLevelStatement(locations, _)
                }
            val newline = if (isDouble) "\n\n" else "\n"
            if (nl.noIndent) newline
            else newline + getIndentation(state.indentation)

          case Provided(ft) => ft.betweenText

          case NoSplit => ""
        }
      }

      def formatWhitespace(implicit sb: StringBuilder): Unit = {

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
              .exists(_.state.split.isNL)

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
          def rightIsCloseDelim =
            right match {
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
            prevState.formatOff

        if (noExtraOffset)
          ws(0)
        else
          style.trailingCommas match {
            // remove comma if no newline
            case TrailingCommas.preserve
                if tok.left.is[T.Comma] && isClosedDelimWithNewline(false) =>
              sb.setLength(sb.length - 1)
              if (!tok.right.is[T.RightParen]) ws(1)
              else if (style.spaces.inParentheses) sb.append(' ')
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
          case _ if tok.meta.left.firstNL < 0 => None
          case _: T.Constant.String =>
            TreeOps.getStripMarginChar(tok.meta.leftOwner).map { pipe =>
              def isPipeFirstChar = text.find(_ != '"').contains(pipe)
              val noAlign = !style.align.stripMargin || prevState.split.isNL
              val pipeOffset =
                if (style.align.stripMargin && isPipeFirstChar) 1 else 0
              val indent = pipeOffset +
                (if (noAlign) prevState.indentation
                 else prevState.prev.column + prevState.prev.split.length)
              (pipe, 2 + indent)
            }
          case _: T.Interpolation.Part =>
            TreeOps.findInterpolate(tok.meta.leftOwner).flatMap { ti =>
              TreeOps.getStripMarginChar(ti).map { pipe =>
                val tiState =
                  locations(tokens(ti.tokens.head).meta.idx).state.prev
                val indent =
                  if (!style.align.stripMargin) tiState.indentation
                  else
                    tiState.column + (ti.parts.headOption match {
                      case Some(Lit.String(x)) if x.headOption.contains(pipe) =>
                        1
                      case _ => 0
                    })
                (pipe, 2 + indent)
              }
            }
          case _ => None
        }
        tupleOpt.fold(text) {
          case (pipe, indent) =>
            val spaces = getIndentation(indent)
            getStripMarginPattern(pipe).matcher(text).replaceAll(spaces)
        }
      }

      def formatComment(implicit sb: StringBuilder): Unit = {
        val text = tok.meta.left.text
        if (isSingleLineComment(text))
          new FormatSlc(text).format
        else if (text == "/**/")
          sb.append(text)
        else if (isDocstring(text))
          formatDocstring(text)
        else
          new FormatMlc(text).format
      }

      private def formatOnelineDocstring(
          text: String
      )(implicit sb: StringBuilder): Boolean = {
        curr.isStandalone && {
          val matcher = onelineDocstring.matcher(text)
          matcher.matches() && (style.docstrings.oneline match {
            case Docstrings.Oneline.fold => true
            case Docstrings.Oneline.unfold => false
            case Docstrings.Oneline.keep =>
              matcher.start(1) == -1 && matcher.start(3) == -1
          }) && {
            val content = matcher.group(2)
            val folding = // 7 is the length of "/** " and " */"
              content.length <= style.maxColumn - prevState.indentation - 7 ||
                (style.docstrings.wrap eq Docstrings.Wrap.no)
            if (folding) sb.append("/** ").append(content).append(" */")
            folding
          }
        }
      }

      private def formatDocstring(
          text: String
      )(implicit sb: StringBuilder): Unit = {
        if (style.docstrings.style.isEmpty) sb.append(text)
        else if (!formatOnelineDocstring(text))
          new FormatMlDoc(text).format
      }

      private abstract class FormatCommentBase(
          protected val extraIndent: Int = 1,
          protected val leadingMargin: Int = 0
      )(implicit sb: StringBuilder) {
        protected final val breakBefore = curr.hasBreakBefore
        protected final val indent =
          if (breakBefore) prevState.indentation
          else prevState.prev.indentation
        // extra 1 is for "*" (in "/*" or " *") or "/" (in "//")
        protected final val maxLength =
          style.maxColumn - indent - extraIndent - 1

        protected final def getFirstLineLength =
          if (breakBefore) 0
          else
            prevState.prev.column - prevState.prev.indentation +
              prevState.split.length

        protected final def canRewrite =
          style.comments.wrap match {
            case Comments.Wrap.no => false
            case Comments.Wrap.trailing => curr.hasBreakAfter
            case Comments.Wrap.standalone => breakBefore && curr.hasBreakAfter
          }

        protected final type WordIter = Iterator[String]
        @tailrec
        protected final def iterWords(
            iter: WordIter,
            appendLineBreak: () => Unit,
            lineLength: Int = 0,
            extraMargin: String = " "
        ): Unit =
          if (iter.hasNext) {
            val word = iter.next()
            val length = word.length
            val maybeNextLineLength = 1 + length +
              (if (lineLength == 0) leadingMargin else lineLength)
            val nextLineLength =
              if (
                lineLength < extraMargin.length ||
                maybeNextLineLength <= maxLength
              ) {
                sb.append(' ')
                maybeNextLineLength
              } else {
                appendLineBreak()
                sb.append(extraMargin)
                length + extraMargin.length
              }
            sb.append(word)
            iterWords(iter, appendLineBreak, nextLineLength, extraMargin)
          }
      }

      private class FormatSlc(text: String)(implicit sb: StringBuilder)
          extends FormatCommentBase {
        def format: Unit = {
          if (
            canRewrite &&
            text.length + indent > style.maxColumn
          ) {
            val useSlc =
              breakBefore && style.comments.wrapStandaloneSlcAsSlc
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
            iterWords(wordIter, appendLineBreak, getFirstLineLength)
            if (!useSlc) sb.append(" */")
          } else sb.append(removeTrailingWhiteSpace(text))
        }
      }

      private class FormatMlc(text: String)(implicit sb: StringBuilder)
          extends FormatCommentBase {
        private val spaces: String = getIndentation(indent + 1)

        def format: Unit = {
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
                override def hasNext: Boolean =
                  hasPara && lineIter.hasNext && {
                    hasPara = !paraEnds
                    if (!hasPara)
                      do lineIter.next() while (lineIter.hasNext && paraEnds)
                    hasPara
                  }
                override def next() =
                  new ParaLineIter().flatMap(splitAsIterator(slcDelim))

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
            iterSections(sectionIter, getFirstLineLength)
            sb.append(" */")
          } else {
            val trimmed = removeTrailingWhiteSpace(text)
            sb.append(leadingAsteriskSpace.matcher(trimmed).replaceAll(spaces))
          }
        }

        private def appendLineBreak(): Unit = {
          sb.append('\n').append(spaces).append('*')
        }

        private type ParaIter = Iterator[WordIter]
        private def iterParagraphs(iter: ParaIter, firstLineLen: Int): Unit = {
          iterWords(iter.next(), appendLineBreak, firstLineLen)
          while (iter.hasNext) {
            appendLineBreak()
            iterWords(iter.next(), appendLineBreak)
          }
        }

        private type SectIter = Iterator[ParaIter]
        private def iterSections(iter: SectIter, firstLineLen: Int): Unit = {
          iterParagraphs(iter.next(), firstLineLen)
          while (iter.hasNext) {
            appendLineBreak()
            appendLineBreak()
            iterParagraphs(iter.next(), 0)
          }
        }
      }

      private class FormatMlDoc(text: String)(implicit sb: StringBuilder)
          extends FormatCommentBase(
            if (style.docstrings.isSpaceAsterisk) 2 else 1,
            if (style.docstrings.isAsteriskSpace) 1 else 0
          ) {
        private val spaces: String = getIndentation(indent + extraIndent)
        private val margin = getIndentation(1 + leadingMargin)

        def format: Unit = {
          val docOpt =
            if (
              (style.docstrings.wrap eq Docstrings.Wrap.yes) &&
              curr.isStandalone
            )
              ScaladocParser.parse(tok.meta.left.text)
            else None
          docOpt.fold(formatNoWrap)(formatWithWrap)
        }

        private def formatWithWrap(doc: Scaladoc): Unit = {
          sb.append("/**")
          if (style.docstrings.skipFirstLine) appendBreak()
          sb.append(' ')
          val sbLen = sb.length()
          val paras = doc.para.iterator
          paras.foreach { para =>
            para.term.foreach { term =>
              if (sb.length() != sbLen) sb.append(margin)
              term match {
                case t: Scaladoc.CodeBlock =>
                  sb.append("{{{")
                  appendBreak()
                  t.code.foreach { x =>
                    if (x.nonEmpty) {
                      val matcher = docstringLeadingSpace.matcher(x)
                      val minMargin = margin.length
                      if (matcher.lookingAt()) {
                        val offset = matcher.end()
                        val extra = math.max(0, offset - minMargin)
                        val codeIndent = minMargin + extra - extra % 2
                        sb.append(getIndentation(codeIndent))
                        sb.append(CharBuffer.wrap(x, offset, x.length))
                      } else
                        sb.append(getIndentation(minMargin)).append(x)
                    }
                    appendBreak()
                  }
                  sb.append(margin).append("}}}")
                  appendBreak()
                case t: Scaladoc.Heading =>
                  val delimiter = t.level * '='
                  sb.append(delimiter).append(t.title).append(delimiter)
                  appendBreak()
                case t: Scaladoc.Tag =>
                  sb.append(t.tag.tag)
                  if (t.tag.hasLabel) sb.append(' ').append(t.label.syntax)
                  if (t.tag.hasDesc) {
                    val words = t.desc.part.iterator.map(_.syntax)
                    val tagMargin = getIndentation(2 + margin.length)
                    // use maxLength to force a newline
                    iterWords(words, appendBreak, maxLength, tagMargin)
                  }
                  appendBreak()
                case t: Scaladoc.ListBlock =>
                  // outputs margin space and appends new line, too
                  // therefore, let's start by "rewinding"
                  if (sb.length() != sbLen || leadingMargin == 0) {
                    sb.setLength(sb.length() - margin.length)
                  } else {
                    // don't output on top line, lists are sensitive to margin
                    sb.setLength(sb.length() - 1) // remove space
                    appendBreak()
                  }
                  formatListBlock(getIndentation(margin.length + 2))(t)
                case t: Scaladoc.Text =>
                  formatTextAfterMargin(t.part.iterator.map(_.syntax))
                case t: Scaladoc.Table => formatTable(t)
              }
            }
            if (paras.hasNext) appendBreak()
          }
          sb.append('/')
        }

        private def formatTextAfterMargin(words: WordIter): Unit = {
          // remove space as iterWords adds it
          sb.setLength(sb.length() - 1)
          iterWords(words, appendBreak, 0, margin)
          appendBreak()
        }

        private def formatListBlock(
            listIndent: String
        )(block: Scaladoc.ListBlock): Unit = {
          val prefix = block.prefix
          val itemIndent = getIndentation(listIndent.length + prefix.length + 1)
          block.item.foreach { x =>
            sb.append(listIndent).append(prefix)
            formatListTerm(itemIndent)(x)
          }
        }

        private def formatListTerm(
            itemIndent: String
        )(item: Scaladoc.ListItem): Unit = {
          val words = item.text.part.iterator.map(_.syntax)
          iterWords(words, appendBreak, itemIndent.length - 1, itemIndent)
          appendBreak()
          item.nested.foreach(formatListBlock(itemIndent))
        }

        private def formatTable(table: Scaladoc.Table): Unit = {
          val rows = table.row.view :+ table.header
          val align = table.align
          val maxCols = rows.map(_.col.length).max
          val colsRange = 0 until maxCols
          val maxLengths = colsRange.map { x =>
            rows.collect { case r if r.col.length > x => r.col(x).length }.max
          }

          @inline def beforeAll: Unit = sb.append(margin)
          @inline def beforeEach: Unit = sb.append('|')
          @inline def afterAll: Unit = { sb.append('|'); appendBreak() }
          @inline def getAlign(col: Int) =
            if (col < align.length) align(col) else Scaladoc.Table.Left
          def formatCols(useMargin: Boolean)(f: Int => Unit): Unit = {
            if (useMargin) beforeAll
            colsRange.foreach { x => beforeEach; f(x) }
            afterAll
          }

          def formatRow(useMargin: Boolean)(row: Scaladoc.Table.Row): Unit =
            formatCols(useMargin) { col =>
              val cell = if (col < row.col.length) row.col(col) else ""
              val pad = maxLengths(col) - cell.length
              val lpad = getAlign(col).leftPad(pad)
              sb.append(getIndentation(1 + lpad))
                .append(cell)
                .append(getIndentation(1 + pad - lpad))
            }
          formatRow(false)(table.header)
          formatCols(true) { col =>
            sb.append(getAlign(col).syntax(maxLengths(col)))
          }
          table.row.foreach(formatRow(true))
        }

        private def formatNoWrap: Unit = {
          // remove "/*" (keep one asterisk) and "*/"
          val trimmed = CharBuffer.wrap(text, 2, text.length - 2)
          val matcher = docstringLine.matcher(trimmed)
          sb.append("/**")
          val sbLen = sb.length()
          var prevWasBlank = style.docstrings.skipFirstLine
          while (matcher.find()) {
            val contentBeg = matcher.start(2)
            val contentEnd = matcher.end(2)
            if (contentBeg == contentEnd) prevWasBlank = true
            else {
              if (sb.length() != sbLen) appendBreak()
              if (prevWasBlank) {
                appendBreak
                prevWasBlank = false
              }
              if (sb.length() == sbLen) sb.append(' ') else sb.append(margin)
              val extraMargin =
                matcher.end(1) - matcher.start(1) - margin.length
              if (extraMargin > 0) sb.append(getIndentation(extraMargin))
              sb.append(CharBuffer.wrap(trimmed, contentBeg, contentEnd))
            }
          }
          appendBreak
          sb.append('/')
        }

        private def appendBreak(): Unit =
          sb.append('\n').append(spaces).append('*')
      }

    }

    /**
      * Returns how many extra spaces are needed to align tokens, as configured
      * by `initStyle.align.tokens`.
      */
    // TODO(olafur) Refactor implementation to make it maintainable. It's super
    // imperative and error-prone right now.
    private def alignmentTokens: Map[Int, Int] = {
      lazy val noAlignTokens = initStyle.align.tokens.isEmpty &&
        styleMap.tok2style.values.forall(_.align.tokens.isEmpty)
      if (locations.length != tokens.length || noAlignTokens)
        Map.empty[Int, Int]
      else {
        var columnShift = 0
        implicit val finalResult = Map.newBuilder[Int, Int]

        // all blocks must be here, to get final flush
        val blocks = new mutable.HashMap[Tree, AlignBlock]
        def createBlock(x: Tree) = blocks.getOrElseUpdate(x, new AlignBlock)
        var prevAlignContainer: Tree = null
        var prevBlock: AlignBlock =
          if (initStyle.align.multiline) null else createBlock(null)
        val getOrCreateBlock: Tree => AlignBlock =
          if (initStyle.align.multiline) createBlock else _ => prevBlock
        val getBlockToFlush: (=> Tree, Boolean) => Option[AlignBlock] =
          if (initStyle.align.multiline) // don't flush unless blank line
            (x, isBlankLine) => if (isBlankLine) blocks.get(x) else None
          else (_, _) => Some(prevBlock)
        val shouldFlush: Tree => Boolean =
          if (!initStyle.align.multiline) _ => true
          else (x: Tree) => x eq prevAlignContainer

        var idx = 0
        while (idx < locations.length) {
          var alignContainer: Tree = null
          val columnCandidates = IndexedSeq.newBuilder[FormatLocation]
          @tailrec
          def processLine: FormatLocation = {
            val floc = locations(idx)
            idx += 1
            val ok = !floc.state.split.isNL
            if (!ok || floc.formatToken.leftHasNewline) columnShift = 0
            columnShift += floc.shift
            if (!ok) floc
            else {
              getAlignContainer(floc).foreach { container =>
                if (alignContainer eq null)
                  alignContainer = container
                if (alignContainer eq container)
                  columnCandidates += floc.copy(
                    alignContainer = container,
                    alignHashKey = getAlignHashKey(floc),
                    shift = floc.state.prev.column + columnShift
                  )
              }
              if (idx < locations.length) processLine else floc
            }
          }

          implicit val location = processLine
          val isBlankLine = location.state.split.modExt.mod.newlines > 1
          if (alignContainer eq null) {
            getBlockToFlush(
              getAlignContainer(location.formatToken.meta.rightOwner),
              isBlankLine
            ).foreach(flushAlignBlock)
          } else {
            val candidates = columnCandidates.result()
            val block = getOrCreateBlock(alignContainer)
            if (block.isEmpty) {
              if (!isBlankLine) block += candidates
            } else {
              val prev = block.last
              val matches =
                columnMatches(prev, candidates, location.formatToken)
              if (matches > 0) {
                // truncate candidates if matches are shorter than both lists
                val truncate =
                  matches < prev.length && matches < candidates.length
                if (truncate) {
                  block(block.length - 1) = prev.take(matches)
                  block += candidates.take(matches)
                } else block += candidates
              }
              if (isBlankLine || matches == 0 && shouldFlush(alignContainer)) {
                flushAlignBlock(block)
                if (!isBlankLine && matches == 0) block += candidates
              }
            }

            prevAlignContainer = alignContainer
            prevBlock = block
          }
        }
        blocks.valuesIterator.foreach(flushAlignBlock)
        finalResult.result()
      }
    }

    private def isEarlierLine(t: Tree)(implicit fl: FormatLocation): Boolean = {
      val idx = tokens(t.tokens.head).meta.idx + 1
      idx <= fl.formatToken.meta.idx && // e.g., leading comments
      locations(idx).leftLineId != fl.leftLineId
    }

    object AlignContainer {
      def unapply(tree: Tree): Option[Tree] =
        tree match {
          case _: Source | _: Template | _: Term.Block | _: Term.Match |
              _: Term.Function | _: Term.PartialFunction =>
            Some(tree)
          case _ => None
        }

      object WithBody {
        def unapply(tree: Tree): Option[Tree] =
          tree match {
            case p: Defn.Def => Some(p.body)
            case p: Defn.Val => Some(p.rhs)
            case p: Defn.Trait => Some(p.templ)
            case p: Defn.Class => Some(p.templ)
            case p: Defn.Object => Some(p.templ)
            case _ => None
          }
      }
    }

    @tailrec
    private def getAlignContainerParent(
        child: Tree,
        maybeParent: Option[Tree] = None
    )(implicit fl: FormatLocation): Tree =
      maybeParent.orElse(child.parent) match {
        case Some(AlignContainer(p)) => p
        case Some(p @ (_: Term.Select | _: Pat.Var)) =>
          getAlignContainerParent(p)
        case Some(p: Term.Apply) if p.fun eq child =>
          getAlignContainerParent(p)
        case Some(p: Term.Apply)
            if p.args.length == 1 && child.is[Term.Apply] =>
          getAlignContainerParent(p)
        // containers that can be traversed further if on same line
        case Some(p @ (_: Case | _: Enumerator)) =>
          if (isEarlierLine(p)) p else getAlignContainerParent(p)
        // containers that can be traversed further if single-stat
        case Some(p @ AlignContainer.WithBody(b)) =>
          if (b.is[Term.Block]) p else getAlignContainerParent(p)
        case Some(p: Term.ForYield) if child ne p.body => p
        case Some(p) => p.parent.getOrElse(p)
        case _ => child
      }

    private def getAlignContainer(t: Tree)(implicit fl: FormatLocation): Tree =
      t match {
        case _: Term.ApplyInfix =>
          TreeOps
            .findTreeWithParentSimple(t)(!_.is[Term.ApplyInfix])
            .map { x =>
              val p = x.parent.get
              if (p.is[Term.Apply]) p.parent.getOrElse(p)
              else getAlignContainerParent(x)
            }
            .getOrElse(t)

        case AlignContainer(t) if fl.formatToken.right.is[T.Comment] => t

        case _: Defn | _: Case => getAlignContainerParent(t, Some(t))

        case _ => getAlignContainerParent(t)
      }

    def getAlignContainer(implicit fl: FormatLocation): Option[Tree] = {
      val ft = fl.formatToken
      val slc = isSingleLineComment(ft.right)
      val code = if (slc) "//" else ft.meta.right.text

      fl.style.alignMap.get(code).flatMap { pattern =>
        val owner = getAlignOwner(ft)
        if (!pattern.matcher(owner.getClass.getName).find()) None
        else if (!slc) Some(getAlignContainer(owner))
        else Some(getAlignContainer(ft.meta.rightOwner))
      }
    }

    private def flushAlignBlock(block: AlignBlock)(implicit
        builder: mutable.Builder[(Int, Int), Map[Int, Int]]
    ): Unit = {
      if (block.length > 1)
        flushMultiEntryAlignBlock(block)
      block.clear()
    }

    private def flushMultiEntryAlignBlock(block: AlignBlock)(implicit
        builder: mutable.Builder[(Int, Int), Map[Int, Int]]
    ): Unit = {
      var column = 0
      val columns = block.map(_.length).max

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
        */
      val previousSeparatorLengthGaps = new Array[Int](block.length)
      while (column < columns) {
        val alignmentUnits = prepareAlignmentInfo(
          block.toIndexedSeq,
          previousSeparatorLengthGaps,
          column
        )

        val widest = alignmentUnits.maxBy(_.width)
        val endIndex = locations.length - 1
        alignmentUnits.foreach { info =>
          import info._
          previousSeparatorLengthGaps(lineIndex) =
            widest.separatorLength - separatorLength
          val offset = widest.width - width
          builder += ftIndex -> offset
          if (column == 0 && initStyle.align.multiline && ftIndex < endIndex)
            shiftStateColumnIndent(ftIndex + 1, offset)
        }
        column += 1
      }
    }

    private def shiftStateColumnIndent(startIdx: Int, offset: Int): Unit = {
      // look for StateColumn; it returns indent=0 for withStateOffset(0)
      val stateIndentOpt = locations(startIdx).state.split.modExt.indents
        .filter(_.hasStateColumn)
        .flatMap(_.withStateOffset(0))
      stateIndentOpt.headOption.foreach { indent =>
        @tailrec
        def updateLocation(idx: Int): Unit = {
          val floc = locations(idx)
          if (indent.notExpiredBy(floc.formatToken)) {
            val state = floc.state
            if (state.split.isNL) {
              locations(idx) = floc.copy(state =
                state.copy(indentation = state.indentation + offset)
              )
            }
            val nextIdx = idx + 1
            if (nextIdx < locations.length) updateLocation(nextIdx)
          }
        }
        updateLocation(startIdx)
      }
    }

  }

  private def isCloseDelimForTrailingCommasMultiple(
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Boolean =
    ft.meta.rightOwner match {
      case x: Importer => x.importees.length > 1
      case _ =>
        val args = getApplyArgs(ft, true)._2
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
      override def apply(tree: Tree): Unit =
        tree match {
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
        val hasNL = toks(i).state.split.isNL
        isMultiline(end, i + 1, if (hasNL) minLines - 1 else minLines)
      }
    val formatToken = toks(i).formatToken

    def checkPackage: Option[Boolean] =
      Some(formatToken.meta.leftOwner)
        .collect { case term: Term.Name => term.parent }
        .flatten
        .collect {
          // package a
          case pkg: Pkg =>
            pkg.stats.headOption

          // package a.b.c
          case select: Term.Select =>
            select.parent.collect {
              case pkg: Pkg => pkg.stats.headOption
            }.flatten
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

  private def getAlignOwner(ft: FormatToken): Tree =
    // Corner case when line ends with comment
    if (isSingleLineComment(ft.right)) ft.meta.leftOwner
    else getAlignOwnerNonComment(ft)

  private def getAlignOwnerNonComment(ft: FormatToken): Tree =
    ft.meta.rightOwner match {
      case name: Term.Name =>
        name.parent match {
          case Some(p: Term.ApplyInfix) => p
          case _ => name
        }
      case x => x
    }

  private def columnsMatch(
      row1: FormatLocation,
      row2: FormatLocation,
      eolTree: Tree
  ): Boolean = {
    // skip checking if row1 and row2 matches if both of them continues to a single line of comment
    // in order to vertical align adjacent single lines of comment.
    // see: https://github.com/scalameta/scalafmt/issues/1242
    val slc1 = isSingleLineComment(row1.formatToken.right)
    val slc2 = isSingleLineComment(row2.formatToken.right)
    if (slc1 || slc2) slc1 && slc2
    else
      (row1.alignContainer eq row2.alignContainer) &&
      (row1.alignHashKey == row2.alignHashKey) && {
        row1.style.align.multiline || {
          val row2Owner = getAlignOwnerNonComment(row2.formatToken)
          val row1Owner = getAlignOwnerNonComment(row1.formatToken)
          def isRowOwner(x: Tree) = (x eq row1Owner) || (x eq row2Owner)
          TreeOps.findTreeWithParentSimple(eolTree)(isRowOwner).isEmpty
        }
      }
  }

  private def columnMatches(
      a: Seq[FormatLocation],
      b: Seq[FormatLocation],
      eol: FormatToken
  ): Int = {
    val endOfLineOwner = eol.meta.rightOwner
    @tailrec
    def iter(pairs: Seq[(FormatLocation, FormatLocation)], cnt: Int): Int =
      pairs.headOption match {
        case Some((r1, r2)) if columnsMatch(r1, r2, endOfLineOwner) =>
          iter(pairs.tail, cnt + 1)
        case _ => cnt
      }
    iter(a.zip(b), 0)
  }

  private def prepareAlignmentInfo(
      block: IndexedSeq[IndexedSeq[FormatLocation]],
      separatorLengthGaps: Array[Int],
      column: Int
  ): Vector[AlignmentUnit] = {
    var i = 0
    val units = Vector.newBuilder[AlignmentUnit]
    while (i < block.length) {
      val line = block(i)
      if (column < line.length) {
        val location = line(column)
        val previousWidth = if (column == 0) 0 else line(column - 1).shift
        val key = location.shift - previousWidth + separatorLengthGaps(i)
        val separatorLength =
          if (location.formatToken.right.is[Token.Comment]) 0
          else location.formatToken.meta.right.text.length
        units += AlignmentUnit(
          key + separatorLength,
          location.formatToken.meta.idx,
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

  type AlignBlock = mutable.ArrayBuffer[IndexedSeq[FormatLocation]]

  case class FormatLocation(
      formatToken: FormatToken,
      state: State,
      style: ScalafmtConfig,
      leftLineId: Int, // only guaranteed to match for toks on the same line
      shift: Int = 0,
      alignContainer: Tree = null,
      alignHashKey: Int = 0,
      replace: String = null
  ) {
    def hasBreakAfter: Boolean = state.split.isNL
    def hasBreakBefore: Boolean =
      state.prev.split.isNL || formatToken.left.start == 0
    def isStandalone: Boolean = hasBreakAfter && hasBreakBefore
  }

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
    */
  case class AlignmentUnit(
      width: Int,
      ftIndex: Int,
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
    buf.toIndexedSeq
  }

  // see if indentation level is cached first
  private def getIndentation(len: Int): String =
    if (len < indentations.length) indentations(len) else " " * len

  private val trailingSpace = Pattern.compile("\\h++$", Pattern.MULTILINE)
  private def removeTrailingWhiteSpace(str: String): String = {
    trailingSpace.matcher(str).replaceAll("")
  }

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
  private val docstringLine =
    Pattern.compile("^(?:\\h*+\\*)?(\\h*+)(.*?)\\h*+$", Pattern.MULTILINE)
  private val onelineDocstring = {
    val empty = "\\h*+(\n\\h*+\\*?\\h*+)*"
    Pattern.compile(s"^/\\*\\*$empty([^*\n\\h](?:[^\n]*[^\n\\h])?)$empty\\*/$$")
  }
  private val docstringLeadingSpace = Pattern.compile("^\\h++")

  @inline
  private def getStripMarginPattern(pipe: Char) =
    if (pipe == '|') leadingPipeSpace else compileStripMarginPattern(pipe)

  @inline
  private def compileStripMarginPattern(pipe: Char) =
    Pattern.compile(s"(?<=\n)\\h*+(?=\\${pipe})")

  private val leadingPipeSpace = compileStripMarginPattern('|')

}
