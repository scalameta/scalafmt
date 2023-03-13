package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.collection.mutable
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
      val ft = repl.ft
      val idx = ft.meta.idx
      val ftOld = arr(idx)
      val rtOld = ftOld.right
      @inline def mapOld() = tokenMap += FormatTokens.thash(rtOld) -> appended
      copySlice(idx)
      def append(): Unit = {
        if (rtOld ne ft.right) mapOld()
        appended += 1
        result += ft
      }
      def remove(): Unit = {
        mapOld()
        val nextIdx = idx + 1
        val nextFt = ftoks.at(nextIdx)
        val rtMeta = nextFt.meta
        mergeWhitespaceLeftToRight(ftOld.meta, rtMeta).foreach { bw =>
          arr(nextIdx) = nextFt.copy(meta = rtMeta.copy(between = bw))
        }
        removed += 1
      }
      if (repl.how eq ReplacementType.Remove) remove() else append()
    }

    if (appended + removed == 0) ftoks
    else {
      copySlice(arr.length)
      val newarr = result.result()
      @tailrec
      def iter(idx: Int): Unit = {
        if (idx < newarr.length) {
          val ft = newarr(idx)
          // reset all indices and set left from previous right (or first left)
          val (left, leftMeta) =
            if (idx == 0) {
              val headft = arr(0)
              headft.left -> headft.meta.left
            } else {
              val prevft = newarr(idx - 1)
              prevft.right -> prevft.meta.right
            }
          val newMeta = ft.meta.copy(idx = idx, left = leftMeta)
          newarr(idx) = ft.copy(left = left, meta = newMeta)
          tokenMap += FormatTokens.thash(left) -> idx
          iter(idx + 1)
        }
      }
      iter(0)

      tokenMap += {
        val ft = newarr.last
        FormatTokens.thash(ft.right) -> ft.meta.idx
      }
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
    implicit val claimed = new mutable.HashMap[Int, Rule]()
    def claimedRule(implicit ft: FormatToken) = claimed.get(ft.meta.idx)
    implicit val tokens = new mutable.ArrayBuffer[Replacement]()
    val leftDelimIndex = new mutable.ListBuffer[(Int, Option[Rule])]()
    val formatOffStack = new mutable.ListBuffer[Boolean]()
    arr.foreach { implicit ft =>
      ft.right match {
        case _: T.LeftBrace | _: T.LeftParen | _: T.LeftBracket =>
          val ldelimIdx = tokens.length
          val formatOff = ft.meta.formatOff
          formatOffStack.prepend(formatOff)
          val ruleOpt =
            if (formatOff) None
            else
              claimedRule match {
                case x @ Some(rule) => if (applyRule(rule)) x else None
                case _ => applyRules
              }
          leftDelimIndex.prepend((ldelimIdx, ruleOpt))
          if (ruleOpt.isEmpty) tokens.append(null)

        case _: T.RightBrace | _: T.RightParen | _: T.RightBracket =>
          val formatOff = formatOffStack.remove(0)
          val (ldelimIdx, ruleOpt) = leftDelimIndex.remove(0)
          if (formatOff && formatOffStack.nonEmpty)
            formatOffStack.update(0, true)
          val replacement = ruleOpt match {
            case Some(rule)
                if !ft.meta.formatOff && claimedRule.forall(_ eq rule) =>
              implicit val style = styleMap.at(ft.right)
              if (rule.enabled) rule.onRight(tokens(ldelimIdx), formatOff)
              else None
            case _ => None
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
          if (!ft.meta.formatOff) applyRules
          else if (formatOffStack.nonEmpty)
            formatOffStack.update(0, true)

        case _ if ft.meta.formatOff =>

        case Whitespace() =>

        case _ => applyRules
      }
    }
    tokens.filter(_ != null)
  }

  private def applyRules(implicit
      ft: FormatToken,
      claimed: mutable.HashMap[Int, Rule],
      tokens: mutable.ArrayBuffer[Replacement]
  ): Option[Rule] = {
    implicit val style = styleMap.at(ft.right)
    FormatTokensRewrite.applyRules(rules)
  }

  private def applyRule(rule: Rule)(implicit
      ft: FormatToken,
      claimed: mutable.HashMap[Int, Rule],
      tokens: mutable.ArrayBuffer[Replacement]
  ): Boolean = {
    implicit val style = styleMap.at(ft.right)
    FormatTokensRewrite.applyRule(rule)
  }

}

object FormatTokensRewrite {

  private val factories = Seq(
    RemoveScala3OptionalBraces,
    ConvertToNewScala3Syntax,
    RemoveEmptyDocstrings,
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
    factories ++ style.rewrite.rules.collect { case x: RuleFactory => x }

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

  private[rewrite] class Replacement(
      val ft: FormatToken,
      val how: ReplacementType,
      // list of FormatToken indices, with the claimed token on the **right**
      val claim: Iterable[Int] = Nil
  )

  private[rewrite] sealed trait ReplacementType
  private[rewrite] object ReplacementType {
    object Remove extends ReplacementType
    object Replace extends ReplacementType
  }

  private[rewrite] def removeToken(implicit ft: FormatToken): Replacement =
    new Replacement(ft, ReplacementType.Remove)

  private[rewrite] def replaceToken(
      text: String,
      owner: Option[Tree] = None,
      claim: Iterable[Int] = Nil
  )(tok: T)(implicit ft: FormatToken): Replacement = {
    val mOld = ft.meta.right
    val mNew = mOld.copy(text = text, owner = owner.getOrElse(mOld.owner))
    val ftNew = ft.copy(right = tok, meta = ft.meta.copy(right = mNew))
    new Replacement(ftNew, ReplacementType.Replace, claim)
  }

  private[rewrite] def replaceTokenBy(
      text: String,
      owner: Option[Tree] = None,
      claim: Iterable[Int] = Nil
  )(f: T => T)(implicit ft: FormatToken): Replacement =
    replaceToken(text, owner, claim)(f(ft.right))

  private[rewrite] def replaceTokenIdent(text: String, t: T)(implicit
      ft: FormatToken
  ): Replacement = replaceToken(text)(
    new T.Ident(t.input, t.dialect, t.start, t.start + text.length, text)
  )

  private def mergeWhitespaceLeftToRight(
      lt: FormatToken.Meta,
      rt: FormatToken.Meta
  ): Option[Array[T]] = {
    import FormatToken.isNL
    val rtBW = rt.between
    val rtNumNL = rt.newlinesBetween
    if (rtNumNL >= 2) None // right has a blank line, nothing to get from left
    else {
      val ltBW = lt.between
      val ltNumNL = lt.newlinesBetween
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

  private def onReplacement(repl: Replacement, rule: Rule)(implicit
      claimed: mutable.HashMap[Int, Rule],
      tokens: mutable.ArrayBuffer[Replacement]
  ): Unit = {
    repl.claim.foreach { claimed.getOrElseUpdate(_, rule) }
    tokens.append(repl)
  }

  private def applyRule(rule: Rule)(implicit
      ft: FormatToken,
      style: ScalafmtConfig,
      claimed: mutable.HashMap[Int, Rule],
      tokens: mutable.ArrayBuffer[Replacement]
  ): Boolean =
    rule.enabled && {
      val res = rule.onToken
      res.foreach(onReplacement(_, rule))
      res.isDefined
    }

  private def applyRules(rules: Seq[Rule])(implicit
      ft: FormatToken,
      style: ScalafmtConfig,
      claimed: mutable.HashMap[Int, Rule],
      tokens: mutable.ArrayBuffer[Replacement]
  ): Option[Rule] = {
    @tailrec
    def iter(remainingRules: Seq[Rule]): Option[Rule] = remainingRules match {
      case r +: rs => if (applyRule(r)) Some(r) else iter(rs)
      case _ => None
    }
    iter(rules)
  }

}
