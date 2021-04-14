package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.Tree
import scala.meta.tokens.{Token => T}

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens
import org.scalafmt.util.StyleMap
import org.scalafmt.util.TokenOps
import org.scalafmt.util.Whitespace

class FormatTokensRewrite(
    ftoks: FormatTokens,
    styleMap: StyleMap,
    rules: Seq[FormatTokensRewrite.Rule]
) {

  import FormatTokensRewrite._

  private val arr = ftoks.arr

  /*
   * The algorithm is as follows:
   * - NB: each rewrite rule acts upon and modifies only the right token
   * - copy tokens which are not rewritten
   * - for rewritten tokens
   *   - add the old token to the lookup map, so we can find the replacement
   *   - for a replaced token, append it
   *   - for a removed token, merge whitespace into the next token
   * - finally, reassign indices and copy left from previous token's right
   */
  def rewrite: FormatTokens = {
    val result = Array.newBuilder[FormatToken]
    result.sizeHint(arr.length)

    val tokenMap = Map.newBuilder[TokenOps.TokenHash, Int]
    tokenMap.sizeHint(arr.length)

    var appended = 0
    var removed = 0
    def copySlice(end: Int): Unit = {
      val beg = appended + removed
      appended += end - beg
      result ++= arr.view.slice(beg, end)
    }
    getRewrittenTokens.foreach { repl =>
      val ft = repl match {
        case Right(ft) => ft
        case Left(idx) => arr(idx)
      }
      copySlice(ft.meta.idx)
      def append: Unit = {
        val oldFt = arr(ft.meta.idx)
        if (oldFt.right ne ft.right)
          tokenMap += FormatTokens.thash(oldFt.right) -> appended
        appended += 1
        result += ft
      }
      def remove: Unit = {
        tokenMap += FormatTokens.thash(ft.right) -> appended
        val nextFt = ftoks.next(ft)
        val rtMeta = nextFt.meta
        mergeWhitespaceLeftToRight(ft.meta, rtMeta).foreach { bw =>
          arr(nextFt.meta.idx) = nextFt.copy(meta = rtMeta.copy(between = bw))
        }
        removed += 1
      }
      if (repl.isLeft) remove else append
    }

    if (appended + removed == 0) ftoks
    else {
      copySlice(arr.length)
      val newarr = result.result()
      @tailrec
      def iter(idx: Int): Unit = {
        if (idx < newarr.length) {
          val ft = newarr(idx)
          if (idx == 0)
            newarr(0) = ft.copy(meta = ft.meta.copy(idx = 0))
          else { // reset all indices and set left from previous right
            val prevft = newarr(idx - 1)
            val newMeta = ft.meta.copy(idx = idx, left = prevft.meta.right)
            newarr(idx) = ft.copy(left = prevft.right, meta = newMeta)
          }
          tokenMap += FormatTokens.thash(newarr(idx).left) -> idx
          iter(idx + 1)
        }
      }
      iter(0)

      tokenMap += FormatTokens.thash(newarr.last.right) -> newarr.last.meta.idx
      new FormatTokens(tokenMap.result())(newarr)
    }
  }

  /*
   * The algorithm is as follows:
   * - NB: each rewrite rule acts upon and modifies only the right token
   * - for every paired token (parens, braces, etc.), keep track of the index
   *   and the rule which applied to the open token, and use the same rule to
   *   the close token; if open wasn't rewritten, skip close; if close refuses
   *   to rewrite, reset the open replacement
   * - also, for paired tokens, observe if there is any intervening format:off
   *   and pass that information to the rule processing the close token; e.g.,
   *   in case of scala3 optional braces, with significant indentation, having
   *   a format:off anywhere between the two braces makes them non-optional
   * - for standalone tokens, simply invoke the rule and record any rewrites
   */
  private def getRewrittenTokens: Iterable[Replacement] = {
    val tokens = new ArrayBuffer[Replacement]()
    val leftDelimIndex = new ListBuffer[(Int, Option[Rule])]()
    val formatOffStack = new ListBuffer[Boolean]()
    arr.foreach { implicit ft =>
      ft.right match {
        case _: T.LeftBrace | _: T.LeftParen | _: T.LeftBracket =>
          val (replacement, ruleOpt) =
            if (ft.meta.formatOff) (null, None)
            else
              applyRules match {
                case None => (null, None)
                case Some((t, rule)) => (t, Some(rule))
              }
          formatOffStack.prepend(ft.meta.formatOff)
          leftDelimIndex.prepend((tokens.length, ruleOpt))
          tokens.append(replacement)

        case _: T.RightBrace | _: T.RightParen | _: T.RightBracket =>
          val formatOff = formatOffStack.remove(0)
          val (ldelimIdx, ruleOpt) = leftDelimIndex.remove(0)
          if (formatOff && formatOffStack.nonEmpty)
            formatOffStack.update(0, true)
          val replacement =
            if (ft.meta.formatOff) None
            else
              ruleOpt.flatMap { rule =>
                implicit val style = styleMap.at(ft.right)
                if (rule.enabled) rule.onRight(tokens(ldelimIdx), formatOff)
                else None
              }
          replacement match {
            case None => tokens(ldelimIdx) = null
            case Some((ltRepl, rtRepl)) =>
              tokens(ldelimIdx) = ltRepl
              tokens.append(rtRepl)
          }

        // above, only paired tokens
        // below, only non-paired tokens

        case _: T.Comment => // formatOff gets set only by comment
          if (formatOffStack.nonEmpty && ft.meta.formatOff)
            formatOffStack.update(0, true)

        case _ if ft.meta.formatOff =>

        case Whitespace() =>

        case _ =>
          applyRules.foreach { case (repl, _) => tokens.append(repl) }
      }
    }
    tokens.filter(_ != null)
  }

  private def applyRules(implicit
      ft: FormatToken
  ): Option[(Replacement, Rule)] = {
    implicit val style = styleMap.at(ft.right)
    @tailrec
    def iter(remainingRules: Seq[Rule]): Option[(Replacement, Rule)] =
      remainingRules.headOption match {
        case None => None
        case Some(rule) =>
          val res = if (rule.enabled) rule.onToken else None
          res match {
            case None => iter(remainingRules.tail)
            case Some(repl) => Some((repl, rule))
          }
      }
    iter(rules)
  }

}

object FormatTokensRewrite {

  private val factories = Seq(
    RemoveScala3OptionalBraces,
    ConvertToNewScala3Syntax,
    RewriteTrailingCommas
  )

  private[rewrite] trait Rule {
    def enabled(implicit style: ScalafmtConfig): Boolean
    // act on or modify only ft.right; process standalone or open (left) delim
    def onToken(implicit
        ft: FormatToken,
        style: ScalafmtConfig
    ): Option[Replacement]
    // act on or modify only ft.right; process close (right) delim
    def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
        ft: FormatToken,
        style: ScalafmtConfig
    ): Option[(Replacement, Replacement)]
  }

  private[rewrite] trait RuleFactory {
    def enabled(implicit style: ScalafmtConfig): Boolean
    def create(ftoks: FormatTokens): Rule
  }

  private def getFactories(implicit style: ScalafmtConfig): Seq[RuleFactory] =
    factories

  def getEnabledFactories(implicit style: ScalafmtConfig): Seq[RuleFactory] =
    getFactories.filter(_.enabled)

  def apply(
      ftoks: FormatTokens,
      styleMap: StyleMap
  ): FormatTokens = {
    val rules = getEnabledFactories(styleMap.init).map(_.create(ftoks))
    if (rules.isEmpty) ftoks
    else new FormatTokensRewrite(ftoks, styleMap, rules).rewrite
  }

  private[rewrite] type Replacement = Either[Int, FormatToken]

  private[rewrite] def removeToken(implicit ft: FormatToken): Replacement =
    Left(ft.meta.idx)

  private[rewrite] def keepToken(implicit ft: FormatToken): Replacement =
    Right(ft)

  private[rewrite] def replaceToken(
      text: String,
      owner: Option[Tree] = None
  )(tok: T)(implicit ft: FormatToken): Replacement = {
    val mOld = ft.meta.right
    val mNew = mOld.copy(text = text, owner = owner.getOrElse(mOld.owner))
    Right(ft.copy(right = tok, meta = ft.meta.copy(right = mNew)))
  }

  private[rewrite] def replaceTokenIdent(text: String, t: T)(implicit
      ft: FormatToken
  ): Replacement = replaceToken(text)(
    new T.Ident(t.input, t.dialect, t.start, t.start + text.length, text)
  )

  @inline
  private def isNL(token: T): Boolean = token.is[T.LF]

  private def mergeWhitespaceLeftToRight(
      lt: FormatToken.Meta,
      rt: FormatToken.Meta
  ): Option[Array[T]] = {
    val rtBW = rt.between
    val rtNumNL = rtBW.count(isNL)
    if (rtNumNL >= 2) None // right has a blank line, nothing to get from left
    else {
      val ltBW = lt.between
      val ltNumNL = ltBW.count(isNL)
      if (rtNumNL >= ltNumNL) None // right has at least as many newlines
      else {
        // left has more newlines than right (so it's non-empty)
        /* for special comment handling: if right ends in a newline, we must
         * end in a newline as well; otherwise, append at least one space */
        val rtEndsInNL = rtNumNL != 0 && isNL(rtBW.last)
        if (rtEndsInNL == isNL(ltBW.last)) Some(ltBW)
        else {
          val numNL = math.min(2, ltNumNL)
          val arr = new Array[T](numNL + (if (rtEndsInNL) 0 else 1))
          // copy just the newlines from left
          ltBW.view.filter(isNL).copyToArray(arr, 0, numNL)
          // copy the space from the right (or create one if rtBW is empty)
          if (!rtEndsInNL) arr(numNL) = rtBW.lastOption.getOrElse {
            val lastNL = arr(numNL - 1)
            new T.Space(lastNL.input, lastNL.dialect, lastNL.start + 1)
          }
          Some(arr)
        }
      }
    }
  }

}
