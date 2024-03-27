package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

import scala.meta.Tree
import scala.meta.tokens.{Token => T}

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens
import org.scalafmt.util.StyleMap
import org.scalafmt.util.TokenOps

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

    val shiftedIndexMap = mutable.Map.empty[Int, Int]

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
      @inline def mapOld(dstidx: Int) =
        tokenMap += FormatTokens.thash(rtOld) -> dstidx
      copySlice(idx)
      def append(): Unit = {
        appended += 1
        result += ft
        if (rtOld ne ft.right) mapOld(appended)
      }
      def remove(dstidx: Int): Unit = {
        mapOld(dstidx)
        val nextIdx = idx + 1
        val nextFt = ftoks.at(nextIdx)
        val rtMeta = nextFt.meta
        mergeWhitespaceLeftToRight(ftOld.meta, rtMeta).foreach { bw =>
          arr(nextIdx) = nextFt.copy(meta = rtMeta.copy(between = bw))
        }
        removed += 1
      }
      repl.how match {
        case ReplacementType.Remove => remove(appended)
        case ReplacementType.Replace => append()
        case r: ReplacementType.RemoveAndResurrect =>
          val rtidx = r.ft.meta.idx
          if (rtidx == idx) { // we moved here
            append()
            shiftedIndexMap.put(idx, appended)
          } else { // we moved from here
            remove(shiftedIndexMap.remove(rtidx).getOrElse(appended))
          }
      }
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
    implicit val session: Session = new Session(rules)
    val tokens = session.tokens
    val leftDelimIndex = new mutable.ListBuffer[Int]()
    val formatOffStack = new mutable.ListBuffer[Boolean]()
    arr.foreach { implicit ft =>
      ft.right match {
        case _: T.LeftBrace | _: T.LeftParen | _: T.LeftBracket =>
          val formatOff = ft.meta.formatOff
          formatOffStack.prepend(formatOff)
          val ldelimIdxOpt =
            if (formatOff) None
            else
              session.claimedRule match {
                case Some(c) => applyRule(c.rule)
                case _ => applyRules
              }
          val ldelimIdx = ldelimIdxOpt.getOrElse(session.claim(null))
          leftDelimIndex.prepend(ldelimIdx)

        case _: T.RightBrace | _: T.RightParen | _: T.RightBracket =>
          val formatOff = formatOffStack.remove(0)
          val ldelimIdx = leftDelimIndex.remove(0)
          if (formatOff && formatOffStack.nonEmpty)
            formatOffStack.update(0, true)
          val left = tokens(ldelimIdx)
          if (left ne null) {
            val ko = ft.meta.formatOff ||
              session.claimedRule.exists(_.rule ne left.rule)
            if (ko) {
              tokens(ldelimIdx) = null
            } else {
              implicit val style = styleMap.at(ft.right)
              left.onRightAndClaim(formatOff, ldelimIdx)
            }
          }

        // above, only paired tokens
        // below, only non-paired tokens

        case _: T.Comment => // formatOff gets set only by comment
          if (!ft.meta.formatOff) applyRules
          else if (formatOffStack.nonEmpty)
            formatOffStack.update(0, true)

        case _ if ft.meta.formatOff =>

        case _: T.Whitespace =>

        case _ => applyRules
      }
    }
    tokens.filter(_ != null)
  }

  private def applyRules(implicit
      ft: FormatToken,
      session: Session
  ): Option[Int] = {
    implicit val style = styleMap.at(ft.right)
    session.applyRules(rules)
  }

  private def applyRule(rule: Rule)(implicit
      ft: FormatToken,
      session: Session
  ): Option[Int] = {
    implicit val style = styleMap.at(ft.right)
    session.applyRule(rule)
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
        session: Session,
        style: ScalafmtConfig
    ): Option[Replacement]
    // act on or modify only ft.right; process close (right) delim
    def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
        ft: FormatToken,
        session: Session,
        style: ScalafmtConfig
    ): Option[(Replacement, Replacement)]

    protected final def removeToken(implicit
        ft: FormatToken,
        style: ScalafmtConfig
    ): Replacement =
      removeToken(Nil)

    protected final def removeToken(claim: Iterable[Int] = Nil)(implicit
        ft: FormatToken,
        style: ScalafmtConfig
    ): Replacement =
      Replacement(this, ft, ReplacementType.Remove, style, claim)

    protected final def replaceToken(
        text: String,
        owner: Option[Tree] = None,
        claim: Iterable[Int] = Nil,
        rtype: ReplacementType = ReplacementType.Replace
    )(tok: T)(implicit ft: FormatToken, style: ScalafmtConfig): Replacement = {
      val mOld = ft.meta.right
      val mNew = mOld.copy(text = text, owner = owner.getOrElse(mOld.owner))
      val ftNew = ft.copy(right = tok, meta = ft.meta.copy(right = mNew))
      Replacement(this, ftNew, rtype, style, claim)
    }

    protected final def replaceTokenBy(
        text: String,
        owner: Option[Tree] = None,
        claim: Iterable[Int] = Nil,
        rtype: ReplacementType = ReplacementType.Replace
    )(f: T => T)(implicit ft: FormatToken, style: ScalafmtConfig): Replacement =
      replaceToken(text, owner, claim, rtype)(f(ft.right))

    protected final def replaceTokenIdent(text: String, t: T)(implicit
        ft: FormatToken,
        style: ScalafmtConfig
    ): Replacement = replaceToken(text)(
      new T.Ident(t.input, t.dialect, t.start, t.start + text.length, text)
    )

  }

  private[rewrite] trait RuleFactory {
    def enabled(implicit style: ScalafmtConfig): Boolean
    def create(implicit ftoks: FormatTokens): Rule
    def priority: Int = 0
  }

  private def getFactories(implicit style: ScalafmtConfig): Seq[RuleFactory] =
    factories ++ style.rewrite.rules.collect { case x: RuleFactory => x }

  def getEnabledFactories(implicit style: ScalafmtConfig): Seq[RuleFactory] =
    getFactories.filter(_.enabled)

  def apply(
      ftoks: FormatTokens,
      styleMap: StyleMap
  ): FormatTokens = {
    val enabledFactories = getEnabledFactories(styleMap.init).sortBy(_.priority)
    val rules = enabledFactories.map(_.create(ftoks))
    if (rules.isEmpty) ftoks
    else new FormatTokensRewrite(ftoks, styleMap, rules).rewrite
  }

  private[rewrite] class Session(rules: Seq[Rule]) {
    private implicit val implicitSession: Session = this
    // map FT index to index in tokens below
    private val claimed = new mutable.HashMap[Int, Int]()
    private[FormatTokensRewrite] val tokens =
      new mutable.ArrayBuffer[Replacement]()

    private[rewrite] def getClaimed(ftIdx: Int): Option[(Int, Replacement)] =
      claimed.get(ftIdx) match {
        case Some(x) =>
          val repl = tokens(x)
          val ok = (repl eq null) || repl.idx == ftIdx
          if (ok) Some((x, repl)) else None
        case _ => None
      }

    @inline
    def claimedRule(implicit ft: FormatToken): Option[Replacement] =
      claimedRule(ft.meta.idx)

    @inline
    private[rewrite] def claimedRule(ftIdx: Int): Option[Replacement] =
      claimed.get(ftIdx).map(tokens.apply).filter(_ ne null)

    private[rewrite] def claim(ftIdx: Int, repl: Replacement): Int = {
      justClaim(ftIdx) {
        if (repl eq null) null
        else
          (repl.how match {
            case rt: ReplacementType.RemoveAndResurrect =>
              val rtidx = rt.ft.meta.idx
              def swapWith(oldidx: Int) = Some {
                tokens(oldidx) = repl.copy(ft = repl.ft.withIdx(rtidx))
                repl.copy(ft = rt.ft.withIdx(repl.idx))
              }
              getClaimed(rtidx) match {
                case Some((oidx, x)) if x != null && x.isRemove =>
                  swapWith(oidx)
                case _ => None
              }
            case _ => None
          }).getOrElse(repl)
      }
    }

    @inline
    private[rewrite] def claim(repl: Replacement)(implicit
        ft: FormatToken
    ): Int =
      claim(ft.meta.idx, repl)

    private def justClaim(ftIdx: Int)(repl: Replacement): Int = {
      val idx = tokens.length
      val claimedIdx = claimed.getOrElseUpdate(ftIdx, idx)
      val preClaimed = claimedIdx < idx
      if (
        preClaimed && {
          val oldrepl = tokens(claimedIdx)
          oldrepl != null && oldrepl.idx == ftIdx
        }
      ) {
        tokens(claimedIdx) = repl
        claimedIdx
      } else {
        if (preClaimed)
          claimed.update(ftIdx, idx)
        tokens.append(repl)
        idx
      }
    }

    private[FormatTokensRewrite] def applyRule(
        attemptedRule: Rule
    )(implicit ft: FormatToken, style: ScalafmtConfig): Option[Int] =
      if (attemptedRule.enabled) attemptedRule.onToken.map { repl =>
        val idx = claim(repl)
        try idx
        finally repl.claim.foreach { claimed.getOrElseUpdate(_, idx) }
      }
      else None

    private[FormatTokensRewrite] def applyRules(
        rules: Seq[Rule]
    )(implicit ft: FormatToken, style: ScalafmtConfig): Option[Int] = {
      @tailrec
      def iter(remainingRules: Seq[Rule]): Option[Int] = remainingRules match {
        case r +: rs =>
          applyRule(r) match {
            case None => iter(rs)
            case x => x
          }
        case _ => None
      }
      iter(rules)
    }

    private[rewrite] def rule[A <: Rule](implicit
        tag: ClassTag[A],
        sc: ScalafmtConfig
    ): Option[A] = {
      val ruleOpt = rules.find(tag.runtimeClass.isInstance)
      ruleOpt.map(_.asInstanceOf[A]).filter(_.enabled)
    }

    private[rewrite] def isRemovedOnLeftOpt(x: FormatToken): Option[Boolean] = {
      val ftIdx = x.meta.idx - 1
      claimedRule(ftIdx).filter(_.idx == ftIdx).map(_.isRemove)
    }

    private[rewrite] def isRemovedOnLeft(x: FormatToken, ok: Boolean): Boolean =
      isRemovedOnLeftOpt(x).contains(ok)

  }

  private[rewrite] case class Replacement(
      rule: Rule,
      ft: FormatToken,
      how: ReplacementType,
      style: ScalafmtConfig,
      // list of FormatToken indices, with the claimed token on the **right**
      claim: Iterable[Int] = Nil
  ) {
    @inline def isRemove: Boolean = how eq ReplacementType.Remove
    @inline def idx: Int = ft.meta.idx

    def onRight(hasFormatOff: Boolean)(implicit
        ft: FormatToken,
        session: Session,
        style: ScalafmtConfig
    ): Option[(Replacement, Replacement)] =
      if (rule.enabled) rule.onRight(this, hasFormatOff) else None

    def onRightAndClaim(hasFormatOff: Boolean, leftIdx: Int)(implicit
        ft: FormatToken,
        session: Session,
        style: ScalafmtConfig
    ): Unit =
      onRight(hasFormatOff) match {
        case None =>
          session.claim(null)
          session.tokens(leftIdx) = null
        case Some((ltRepl, rtRepl)) =>
          session.claim(rtRepl)
          session.tokens(leftIdx) = ltRepl
      }
  }

  private[rewrite] sealed trait ReplacementType
  private[rewrite] object ReplacementType {
    object Remove extends ReplacementType
    object Replace extends ReplacementType
    class RemoveAndResurrect(val ft: FormatToken) extends ReplacementType
  }

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

}
