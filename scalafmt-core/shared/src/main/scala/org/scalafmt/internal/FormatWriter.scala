package org.scalafmt.internal

import java.nio.CharBuffer
import java.util.regex.Pattern

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.{Formatted, Scalafmt}
import org.scalafmt.config.{Comments, Docstrings, Newlines, ScalafmtConfig}
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util.TokenOps._
import org.scalafmt.util.{LiteralOps, TreeOps}

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.mutable
import scala.meta.internal.Scaladoc
import scala.meta.internal.parsers.ScaladocParser
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

/** Produces formatted output from sequence of splits.
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
      val formatToken = location.formatToken

      formatToken.left match {
        case _ if entry.previous.formatToken.meta.formatOff =>
          sb.append(formatToken.meta.left.text) // checked the state for left
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
          def checkApply(t: Tree): Boolean = t.parent match {
            case Some(Term.Apply(_, List(`t`))) => true
            case _ => false
          }
          val ok = tok.meta.leftOwner match {
            case b: Term.Block =>
              checkApply(b) && RedundantBraces.canRewriteWithParens(b) &&
                b.parent.exists(_.tokens.last.start == rb.start)
            case f: Term.Function =>
              checkApply(f) && RedundantBraces.canRewriteWithParens(f)
            case _ => false
          }
          if (ok) {
            val beg = tokens(tokens.matching(rb))
            lookup.update(beg.meta.idx, tok.meta.idx -> loc.leftLineId)
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
              needTemplateBodyBlank(locations, i) ||
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

          val nextNonCommentTok = tokens.nextNonComment(tok)
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
            previous.formatToken.meta.formatOff

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
        def offset = style.indent.main
        val tupleOpt = tok.left match {
          case _ if !style.assumeStandardLibraryStripMargin => None
          case _ if tok.meta.left.firstNL < 0 => None
          case _: T.Constant.String =>
            TreeOps.getStripMarginChar(tok.meta.leftOwner).map { pipe =>
              def isPipeFirstChar = text.find(_ != '"').contains(pipe)
              val noAlign = !style.align.stripMargin ||
                tok.meta.idx <= 1 || prevState.split.isNL
              def alignPipeOffset = if (isPipeFirstChar) 3 else 2
              val thisOffset =
                if (style.align.stripMargin) alignPipeOffset else offset
              val prevIndent =
                if (noAlign) prevState.indentation
                else prevState.prev.column + prevState.prev.split.length
              (pipe, thisOffset + prevIndent)
            }
          case _: T.Interpolation.Part =>
            TreeOps.findInterpolate(tok.meta.leftOwner).flatMap { ti =>
              TreeOps.getStripMarginChar(ti).map { pipe =>
                def alignPipeOffset = ti.parts.headOption match {
                  case Some(Lit.String(x)) if x.headOption.contains(pipe) => 3
                  case _ => 2
                }
                val tiState =
                  locations(tokens(ti.tokens.head).meta.idx).state.prev
                val indent =
                  if (style.align.stripMargin) tiState.column + alignPipeOffset
                  else tiState.indentation + offset
                (pipe, indent)
              }
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
        if (style.docstrings.style eq Docstrings.Preserve) sb.append(text)
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
          val trimmed = removeTrailingWhiteSpace(text)
          if (!canRewrite) sb.append(trimmed)
          else {
            val hasSpace = trimmed.length <= 2 ||
              Character.isWhitespace(trimmed.charAt(2))
            val column = indent + trimmed.length + (if (hasSpace) 0 else 1)
            if (column > style.maxColumn) reFormat(trimmed)
            else if (hasSpace) sb.append(trimmed)
            else sb.append(s"// ${trimmed.substring(2)}")
          }
        }
        private def reFormat(text: String): Unit = {
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
            if (style.docstrings.style eq Docstrings.SpaceAsterisk) 2 else 1,
            if (style.docstrings.style eq Docstrings.AsteriskSpace) 1 else 0
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
            para.term.foreach { term =>
              if (sb.length != sbLen) sb.append(margin)
              term match {
                case t: Scaladoc.CodeBlock =>
                  sb.append("{{{")
                  if (t.code.headOption.exists(_.endsWith("// scala")))
                    formatScalaCodeBlock(t.code)
                  else formatCodeBlock(t.code)
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
                  if (sb.length != sbLen || leadingMargin == 0) {
                    sb.setLength(sb.length - margin.length)
                  } else {
                    // don't output on top line, lists are sensitive to margin
                    sb.setLength(sb.length - 1) // remove space
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
          if (sb.length == sbLen) sb.append('*')
          sb.append('/')
        }

        private def formatTextAfterMargin(words: WordIter): Unit = {
          // remove space as iterWords adds it
          sb.setLength(sb.length - 1)
          iterWords(words, appendBreak, 0, margin)
          appendBreak()
        }

        private def formatCodeBlock(code: Seq[String]): Unit = {
          appendBreak()
          code.foreach { x =>
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
        }

        private def formatScalaCodeBlock(code: Seq[String]): Unit = {
          val codeStyle = style.copy(
            runner = style.runner.copy(
              debug = false,
              eventCallback = null,
              parser = meta.parsers.Parse.parseSource
            ),
            // let's not wrap docstrings, to avoid recursion
            docstrings = style.docstrings.copy(wrap = Docstrings.Wrap.no),
            maxColumn = style.maxColumn - spaces.length - margin.length - 1
          )
          Scalafmt.format(code.mkString("\n"), codeStyle) match {
            case Formatted.Success(formattedCode) =>
              formatCodeBlock(formattedCode.split('\n'))
            case _ => formatCodeBlock(code)
          }
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
          val sbLen = sb.length
          @tailrec
          def iter(prevWasBlank: Boolean): Unit = if (matcher.find()) {
            val contentBeg = matcher.start(2)
            val contentEnd = matcher.end(2)
            if (contentBeg == contentEnd) iter(true)
            else {
              if (sb.length != sbLen) {
                if (prevWasBlank) appendBreak()
                appendBreakWithMargin()
              } else {
                if (style.docstrings.skipFirstLineIf(prevWasBlank))
                  appendBreakWithMargin()
                else sb.append(' ')
              }
              val extraMargin =
                matcher.end(1) - matcher.start(1) - margin.length
              if (extraMargin > 0) sb.append(getIndentation(extraMargin))
              sb.append(CharBuffer.wrap(trimmed, contentBeg, contentEnd))
              iter(false)
            }
          }
          iter(false)
          appendBreak().append('/')
        }

        @inline private def appendBreak() =
          sb.append('\n').append(spaces).append('*')
        @inline private def appendBreakWithMargin() =
          appendBreak().append(margin)
      }

    }

    /** Returns how many extra spaces are needed to align tokens, as configured
      * by `initStyle.align.tokens`.
      */
    // TODO(olafur) Refactor implementation to make it maintainable. It's super
    // imperative and error-prone right now.
    private def alignmentTokens: Map[Int, Int] = {
      lazy val noAlignTokens = styleMap.forall(_.align.tokens.isEmpty)
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
          val columnCandidates = IndexedSeq.newBuilder[AlignStop]
          @tailrec
          def processLine: FormatLocation = {
            if (idx > 0) {
              val prevFloc = locations(idx - 1)
              if (prevFloc.hasBreakAfter || prevFloc.formatToken.leftHasNewline)
                columnShift = 0
            }
            val floc = locations(idx)
            idx += 1
            columnShift += floc.shift
            if (floc.hasBreakAfter || floc.formatToken.leftHasNewline) floc
            else {
              getAlignContainer(floc).foreach { container =>
                if (alignContainer eq null)
                  alignContainer = container
                if (alignContainer eq container)
                  columnCandidates += new AlignStop(
                    getAlignColumn(floc) + columnShift,
                    floc.copy(
                      alignContainer = container,
                      alignHashKey = getAlignHashKey(floc)
                    )
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
            def appendToBlock(line: IndexedSeq[AlignStop], matches: Int = 0) = {
              val eolColumn = location.state.prev.column + columnShift
              val alignLine = new AlignLine(line, eolColumn)
              if (!block.tryAppendToBlock(alignLine, matches)) {
                flushAlignBlock(block)
                block.tryAppendToBlock(alignLine, 0)
              }
            }
            if (block.isEmpty) {
              if (!isBlankLine) appendToBlock(candidates)
            } else {
              val matches =
                columnMatches(block.refStops, candidates, location.formatToken)
              if (matches > 0) appendToBlock(candidates, matches)
              if (isBlankLine || matches == 0 && shouldFlush(alignContainer)) {
                flushAlignBlock(block)
                if (!isBlankLine && matches == 0) appendToBlock(candidates)
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
      val idx = tokens.after(t.tokens.head).meta.idx + 1
      idx <= fl.formatToken.meta.idx && // e.g., leading comments
      locations(idx).leftLineId != fl.leftLineId
    }

    object AlignContainer {
      def unapply(tree: Tree): Option[Tree] =
        tree match {
          case _: Source | _: Template | _: Term.Block | _: Term.Match |
              _: Type.Match | _: Term.Function | _: Term.PartialFunction =>
            Some(tree)
          case _ => None
        }

      object WithBody {
        def unapply(tree: Tree): Option[Tree] =
          tree match {
            case p: Defn.Def => Some(p.body)
            case p: Defn.Given => Some(p.templ)
            case p: Defn.GivenAlias => Some(p.body)
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
      if (block.hasMultiple)
        flushMultiEntryAlignBlock(block)
      block.clear()
    }

    private def flushMultiEntryAlignBlock(block: AlignBlock)(implicit
        builder: mutable.Builder[(Int, Int), Map[Int, Int]]
    ): Unit = {
      val endIndex = locations.length - 1
      block.foreach { x =>
        val headStop = x.stops.head
        if (headStop.floc.style.align.multiline) {
          val offset = block.stopColumns.head - headStop.column
          val ftIndex = headStop.floc.formatToken.meta.idx
          if (ftIndex < endIndex) shiftStateColumnIndent(ftIndex + 1, offset)
        }
        var previousShift = 0
        x.stops.zip(block.stopColumns).foreach { case (stop, blockStop) =>
          val currentShift = blockStop - stop.column
          val offset = currentShift - previousShift
          previousShift = currentShift
          builder += stop.floc.formatToken.meta.idx -> offset
        }
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
        val args = getApplyArgs(ft, true).args
        args.length > 1 && (args.last match {
          case _: Term.Repeated => false
          case t: Term.Param => !t.decltpe.exists(_.is[Type.Repeated])
          case _ => true
        })
    }

  lazy val (
    topLevelHeadTokens,
    topLevelLastToHeadTokens,
    templateBodyTokens
  ) = {
    val headBuffer = Set.newBuilder[Int]
    val lastBuffer = Map.newBuilder[Int, Int]
    val templateBodyBuffer = Map.newBuilder[Int, (Boolean, Template)]
    val trav = new Traverser {
      override def apply(tree: Tree): Unit =
        tree match {
          case _: Term.Block =>
          case t: Template =>
            t.stats.headOption.foreach { x =>
              val idx = leadingComment(x).meta.idx
              templateBodyBuffer += idx -> (true, t)
            }
            t.stats.lastOption.foreach { x =>
              val idx = tokens.getLast(x).meta.idx
              templateBodyBuffer += idx -> (false, t)
            }
            super.apply(t.stats) // skip inits
          case TreeOps.MaybeTopLevelStat(t) =>
            val leading = leadingComment(t).meta.idx
            val trailing = tokens.getLast(t).meta.idx
            headBuffer += leading
            lastBuffer += trailing -> leading
            super.apply(tree)
          case _ =>
            super.apply(tree)
        }
    }

    trav(topSourceTree)
    (headBuffer.result(), lastBuffer.result(), templateBodyBuffer.result())
  }

  private def needTemplateBodyBlank(
      toks: Array[FormatLocation],
      i: Int
  ): Boolean =
    templateBodyTokens.get(i).exists { case (isBefore, template) =>
      val floc = toks(i)
      val settings = floc.style.newlines
      def checkBeforeAfter(ba: Newlines.BeforeAfter): Boolean =
        template.stats.lengthCompare(settings.templateBodyMinStatements) >= 0 &&
          settings.templateBodyIfMinStatements.contains(ba)
      if (isBefore)
        checkBeforeAfter(Newlines.before) ||
        settings.beforeTemplateBodyIfBreakInParentCtors && {
          val curly = tokens(templateCurlyOrLastNonTrivial(template)).meta.idx
          val beforeTemplate = leadingComment(template).meta.idx
          toks(curly).leftLineId != toks(beforeTemplate).leftLineId
        }
      else
        checkBeforeAfter(Newlines.after)
    }

  private def isMultilineTopLevelStatement(
      toks: Array[FormatLocation],
      i: Int
  ): Boolean = {
    @tailrec def isMultiline(end: FormatToken, i: Int, minLines: Int): Boolean =
      if (minLines <= 0) true
      else if (i >= toks.length || toks(i).formatToken == end) false
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
            select.parent.collect { case pkg: Pkg =>
              pkg.stats.headOption
            }.flatten
        }
        .flatten
        .map {
          case pkg: Pkg => tokens.getLast(pkg.ref).right.is[T.LeftBrace]
          case _ => true
        }

    def checkTopLevelStatement: Boolean =
      topLevelHeadTokens.contains(formatToken.meta.idx) && {
        val nextNonCommentTok = tokens.nextNonComment(formatToken)
        val distance = nextNonCommentTok.meta.idx - formatToken.meta.idx
        val nonCommentOwner = nextNonCommentTok.meta.rightOwner match {
          case mod: Mod => mod.parent.get
          case x => x
        }
        isMultiline(
          tokens.getLast(nonCommentOwner),
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
      a: Seq[AlignStop],
      b: Seq[AlignStop],
      eol: FormatToken
  ): Int = {
    val endOfLineOwner = eol.meta.rightOwner
    @tailrec
    def iter(pairs: Seq[(AlignStop, AlignStop)], cnt: Int): Int =
      pairs.headOption match {
        case Some((r1, r2)) if columnsMatch(r1.floc, r2.floc, endOfLineOwner) =>
          iter(pairs.tail, cnt + 1)
        case _ => cnt
      }
    iter(a.zip(b), 0)
  }

}

object FormatWriter {

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
      // first token is BOF
      formatToken.meta.idx <= 1 || state.prev.split.isNL
    def isStandalone: Boolean = hasBreakAfter && hasBreakBefore
  }

  class AlignStop(val column: Int, val floc: FormatLocation)

  class AlignLine(var stops: IndexedSeq[AlignStop], val eolColumn: Int)

  class AlignBlock(
      buffer: mutable.ArrayBuffer[AlignLine] =
        new mutable.ArrayBuffer[AlignLine],
      var refStops: Seq[AlignStop] = Seq.empty,
      var stopColumns: IndexedSeq[Int] = IndexedSeq.empty
  ) {
    def tryAppendToBlock(line: AlignLine, matches: Int): Boolean = {
      // truncate if matches are shorter than both lists
      val truncate = shouldTruncate(line, matches)
      def trunc[A](s: IndexedSeq[A], c: Boolean) = if (c) s.take(matches) else s
      val oldStops = trunc(stopColumns, truncate < 0)
      val newStops = trunc(line.stops, truncate > 0)

      // compute new stops for the block
      var oldShift = 0
      var newShift = 0
      val builder = IndexedSeq.newBuilder[Int]
      // common stops first
      oldStops.zip(newStops).foreach { case (oldStopColumn, newStop) =>
        val oldStopShifted = oldShift + oldStopColumn
        val newStopShifted = newShift + newStop.column
        val diff = newStopShifted - oldStopShifted
        if (diff > 0) {
          oldShift += diff
          builder += newStopShifted
        } else {
          newShift -= diff
          builder += oldStopShifted
        }
      }
      // whatever remains
      oldStops.drop(newStops.length).foreach(builder += oldShift + _)
      newStops.drop(oldStops.length).foreach(builder += newShift + _.column)
      val newStopColumns = builder.result()

      // check overflow
      val style = line.stops.last.floc.style
      if (!style.align.allowOverflow) {
        val overflow = (line.eolColumn + newShift) > style.maxColumn ||
          buffer.exists { x =>
            val idx = -1 + {
              if (truncate < 0) math.min(matches, x.stops.length)
              else x.stops.length
            }
            val lastStop = x.stops(idx)
            val totalShift = newStopColumns(idx) - lastStop.column
            x.eolColumn + totalShift > lastStop.floc.style.maxColumn
          }
        if (overflow) return false // RETURNING
      }

      // now we mutate
      line.stops = newStops
      if (truncate < 0) foreach(x => x.stops = x.stops.take(matches))
      if (newStopColumns.length == newStops.length) refStops = newStops
      buffer += line
      stopColumns = newStopColumns
      true
    }

    // <0 old, 0 neither, >0 new
    private def shouldTruncate(line: AlignLine, matches: Int): Int = {
      // truncate if matches are shorter than both lists
      val oldStops = refStops.length
      val newStops = line.stops.length
      if (matches == 0 || matches >= oldStops || matches >= newStops) 0
      else if (oldStops < newStops) -1 // new is longer
      else if (oldStops > newStops || hasMultiple) 1 // old is longer
      else if (line.stops.last.floc.formatToken.right.is[T.Comment]) 1
      else -1
    }

    def clear(): Unit = {
      buffer.clear()
      refStops = Seq.empty
      stopColumns = IndexedSeq.empty
    }

    @inline def isEmpty: Boolean = buffer.isEmpty
    @inline def hasMultiple: Boolean = buffer.lengthCompare(1) > 0
    @inline def foreach[A](f: AlignLine => A): Unit = buffer.foreach(f)
  }

  /** Separator length gap needed to align blocks with different token
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
  def getAlignColumn(floc: FormatLocation): Int = {
    // if we didn't care about align token lengths, we'd always "useLeft"
    val useLeft = floc.formatToken.right.is[T.Comment]
    if (useLeft) floc.state.prev.column else floc.state.column
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
