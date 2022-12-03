package org.scalafmt.config

import scala.meta._

import org.scalafmt.config.Newlines._
import org.scalafmt.internal.FormatToken
import org.scalafmt.util.TreeOps

import metaconfig._
import metaconfig.generic.Surface

/** @param penalizeSingleSelectMultiArgList
  *   If true, adds a penalty to newlines before a dot starting a select chain
  *   of length one and argument list. The penalty matches the number of
  *   arguments to the select chain application.
  *   {{{
  *     // If true, favor
  *     logger.elem(a,
  *                 b,
  *                 c)
  *     // instead of
  *     logger
  *       .elem(a, b, c)
  *
  *     // penalty is proportional to argument count, example:
  *     logger.elem(a, b, c)    // penalty 2
  *     logger.elem(a, b, c, d) // penalty 3, etc.
  *   }}}
  *
  * If false, matches pre-v0.5 behavior. Note. this option may be removed in a
  * future release.
  * @param neverBeforeJsNative
  *   If true, a newline will never be placed in front of js.native.
  * @param sometimesBeforeColonInMethodReturnType
  *   If true, scalafmt may choose to put a newline before colon : at defs.
  * @param beforeCurlyLambdaParams
  *   - if Never, tries to use a space between the opening curly brace and the
  *     list of parameters of anonymous functions, and some partial functions
  *     (those with a single case clause and no conditions)
  *   - if MultilineWithCaseOnly, forces a newline in partial functions (see
  *     above) which can't be formatted on a single line
  *   - if Always, forces a newline in lambda and partial functions. For
  *     example:
  *     {{{
  *   something.map {
  *     n =>
  *       consume(n)
  *   }
  *     }}}
  * @param afterCurlyLambdaParams
  *   - If `never` (default), it will remove any extra lines below curly lambdas
  *     {{{
  *   something.map { x =>
  *
  *     f(x)
  *   }
  *     }}}
  *     will become
  *     {{{
  *   something.map { x =>
  *     f(x)
  *   }
  *     }}}
  *   - If `always`, it will always add one empty line (opposite of `never`). If
  *     `preserve`, and there isn't an empty line, it will keep it as it is. If
  *     there is one or more empty lines, it will place a single empty line.
  *   - If `squash`, it will try to squash lambda body in one line:
  *     {{{
  *     xs.map { x =>
  *       x + 1
  *     }
  *     }}}
  *     will become
  *     {{{
  *     xs.map { x => x + 1 }
  *     }}}
  * @param alwaysBeforeElseAfterCurlyIf
  *   if true, add a new line between the end of a curly if and the following
  *   else. For example
  *   {{{
  *   if(someCond) {
  *     // ...
  *   }
  *   else //...
  *   }}}
  * @param beforeMultilineDef
  *   If unfold (or true), add a newline before the body of a multiline def
  *   without curly braces. See #1126 for discussion. For example,
  *   {{{
  *     // newlines.beforeMultilineDef = fold
  *     def foo(bar: Bar): Foo = bar
  *       .flatMap(f)
  *       .map(g)
  *
  *     // newlines.beforeMultilineDef = unfold
  *     def foo(bar: Bar): Foo =
  *       bar
  *         .flatMap(f)
  *         .map(g)
  *   }}}
  * @param avoidAfterYield
  *   If false (legacy behavior), inserts unconditional line break after `yield`
  *   if the yield body doesn't fit on a single line. For example,
  *   {{{
  *     // newlines.avoidAfterYield = true (default)
  *     for (a <- as)
  *     yield Future {
  *       ...
  *     }
  *
  *     // newlines.avoidAfterYield = false (default before v2).
  *     for (a <- as)
  *     yield
  *       Future {
  *         ...
  *       }
  *   }}}
  * @param source
  *   Controls how line breaks in the input source are handled
  *   - If `classic` (default), the old mixed behaviour will be used
  *   - If `keep`, try to keep source newlines
  *   - If `fold`, ignore source and try to remove line breaks
  *   - If `unfold`, ignore source and try to break lines
  * @param afterInfix
  *   Controls how line breaks around operations are handled
  *   - If `keep` (default for source=classic,keep), preserve existing
  *   - If `some` (default for source=fold), break after some infix ops
  *   - If `many` (default for source=unfold), break after many infix ops
  * @param afterInfixBreakOnNested
  *   Force breaks around nested (enclosed in parentheses) expressions
  * @param afterInfixMaxCountPerFile
  *   Switch to `keep` for a given file if the total number of infix operations
  *   in that file exceeds this value
  * @param afterInfixMaxCountPerExprForSome
  *   Switch to `many` for a given expression (possibly nested) if the number of
  *   operations in that expression exceeds this value AND `afterInfix` had been
  *   set to `some`.
  * @param topLevelStatementBlankLines
  *   Controls blank line before and/or after a top-level statement.
  * @param avoidForSimpleOverflow
  *   - punct: don't force break if overflow is only due to trailing punctuation
  *   - tooLong: don't force break if overflow is due to tokens which are too
  *     long and would likely overflow even after a break
  */
case class Newlines(
    source: SourceHints = Newlines.classic,
    @annotation.ExtraName("neverInResultType")
    avoidInResultType: Boolean = false,
    beforeTypeBounds: SourceHints = Newlines.classic,
    neverBeforeJsNative: Boolean = false,
    sometimesBeforeColonInMethodReturnType: Boolean = true,
    private[config] val beforeOpenParenDefnSite: Option[BeforeOpenParen] = None,
    private[config] val beforeOpenParenCallSite: Option[BeforeOpenParen] = None,
    penalizeSingleSelectMultiArgList: Boolean = true,
    beforeCurlyLambdaParams: BeforeCurlyLambdaParams =
      BeforeCurlyLambdaParams.never,
    private val topLevelStatementBlankLines: Seq[TopStatBlanks] = Seq.empty,
    @annotation.DeprecatedName(
      "topLevelStatementsMinBreaks",
      "Use newlines.topLevelStatementBlankLines instead",
      "3.0.0"
    )
    private val topLevelStatementsMinBreaks: Int = 1,
    @annotation.DeprecatedName(
      "topLevelStatements",
      "Use newlines.topLevelStatementBlankLines instead",
      "3.0.0"
    )
    private val topLevelStatements: Seq[BeforeAfter] = Seq.empty,
    beforeTemplateBodyIfBreakInParentCtors: Boolean = false,
    topLevelBodyIfMinStatements: Seq[BeforeAfter] = Seq.empty,
    topLevelBodyMinStatements: Int = 2,
    @annotation.ExtraName("afterCurlyLambda")
    afterCurlyLambdaParams: AfterCurlyLambdaParams =
      AfterCurlyLambdaParams.never,
    @annotation.ExtraName("usingParamListModifierForce")
    implicitParamListModifierForce: Seq[BeforeAfter] = Seq.empty,
    @annotation.ExtraName("usingParamListModifierPrefer")
    implicitParamListModifierPrefer: Option[BeforeAfter] = None,
    alwaysBeforeElseAfterCurlyIf: Boolean = false,
    forceBeforeAssign: ForceBeforeMultilineAssign =
      ForceBeforeMultilineAssign.never,
    private val forceBeforeMultilineAssign: Option[ForceBeforeMultilineAssign] =
      None,
    @annotation.DeprecatedName(
      "alwaysBeforeMultilineDef",
      "Use newlines.forceBeforeMultilineAssign instead",
      "3.0.0"
    )
    private val alwaysBeforeMultilineDef: Boolean = false,
    private[config] val beforeMultiline: Option[SourceHints] = None,
    @annotation.DeprecatedName(
      "beforeMultilineDef",
      "Use newlines.beforeMultiline, newlines.forceBeforeMultilineAssign instead",
      "3.0.0"
    )
    beforeMultilineDef: Option[SourceHints] = None,
    private[config] val selectChains: Option[SourceHints] = None,
    afterInfix: Option[AfterInfix] = None,
    afterInfixBreakOnNested: Boolean = false,
    afterInfixMaxCountPerExprForSome: Int = 10,
    afterInfixMaxCountPerFile: Int = 500,
    avoidForSimpleOverflow: Seq[AvoidForSimpleOverflow] = Seq.empty,
    inInterpolation: InInterpolation = InInterpolation.allow,
    avoidAfterYield: Boolean = true
) {
  if (
    implicitParamListModifierForce.nonEmpty &&
    implicitParamListModifierPrefer.nonEmpty
  ) {
    throw new ScalafmtConfigException(
      "can't specify both " +
        "implicitParamListModifierForce and implicitParamListModifierPrefer"
    )
  }

  @inline
  def sourceIgnored: Boolean = source.ignoreSourceSplit

  @inline
  def keepBreak(newlines: => Int): Boolean =
    source.eq(Newlines.keep) && newlines != 0

  @inline
  def keepBreak(ft: FormatToken): Boolean =
    keepBreak(ft.newlinesBetween)

  val breakAfterInfix: AfterInfix =
    afterInfix.getOrElse {
      source match {
        case Newlines.unfold => AfterInfix.many
        case Newlines.fold => AfterInfix.some
        case Newlines.keep => AfterInfix.keep
        case Newlines.classic => AfterInfix.keep
      }
    }
  val formatInfix: Boolean = breakAfterInfix ne AfterInfix.keep

  def checkInfixConfig(infixCount: Int): Newlines = {
    if (infixCount <= afterInfixMaxCountPerFile || !formatInfix) this
    else copy(afterInfix = Some(AfterInfix.keep))
  }

  lazy val forceBeforeImplicitParamListModifier: Boolean =
    implicitParamListModifierForce.contains(before)
  lazy val forceAfterImplicitParamListModifier: Boolean =
    implicitParamListModifierForce.contains(after)

  private def preferBeforeImplicitParamListModifier: Boolean =
    implicitParamListModifierPrefer.contains(before)
  lazy val notPreferAfterImplicitParamListModifier: Boolean =
    implicitParamListModifierForce.nonEmpty ||
      preferBeforeImplicitParamListModifier
  lazy val notBeforeImplicitParamListModifier: Boolean =
    if (implicitParamListModifierForce.isEmpty)
      !preferBeforeImplicitParamListModifier
    else
      !forceBeforeImplicitParamListModifier

  lazy val avoidForSimpleOverflowPunct: Boolean =
    avoidForSimpleOverflow.contains(AvoidForSimpleOverflow.punct)
  lazy val avoidForSimpleOverflowTooLong: Boolean =
    avoidForSimpleOverflow.contains(AvoidForSimpleOverflow.tooLong)
  lazy val avoidForSimpleOverflowSLC: Boolean =
    avoidForSimpleOverflow.contains(AvoidForSimpleOverflow.slc)

  @inline
  def alwaysBeforeCurlyLambdaParams =
    beforeCurlyLambdaParams eq BeforeCurlyLambdaParams.always

  lazy val getBeforeMultiline = beforeMultiline.getOrElse(source)
  lazy val shouldForceBeforeMultilineAssign = {
    forceBeforeMultilineAssign.getOrElse {
      val useDef = alwaysBeforeMultilineDef ||
        beforeMultilineDef.contains(Newlines.unfold)
      if (useDef) ForceBeforeMultilineAssign.`def`
      else ForceBeforeMultilineAssign.never
    }
  }

  lazy val getSelectChains = selectChains.getOrElse(source)

  private lazy val topStatBlankLinesSorted = {
    if (topLevelStatementBlankLines.isEmpty) {
      val nb = NumBlanks(
        if (topLevelStatements.contains(before)) 1 else 0,
        if (topLevelStatements.contains(after)) 1 else 0
      )
      if (nb.isEmpty) Seq.empty
      else {
        val pattern = Some("^Pkg|^Defn\\.|^Decl\\.")
        Seq(TopStatBlanks(pattern, topLevelStatementsMinBreaks, Some(nb)))
      }
    } else {
      /* minBreaks has to come first; since we'll be adding blanks, this could
       * potentially move us into another setting which didn't match before we
       * we added the blanks; the rest are sorted to put more specific first */
      topLevelStatementBlankLines.filter(x => x.minNest <= x.maxNest).sortBy {
        x => (x.minBreaks, x.maxNest, -x.minNest, x.regex.fold(0)(-_.length))
      }
    }
  }

  @inline def hasTopStatBlankLines = topStatBlankLinesSorted.nonEmpty

  def getTopStatBlankLines(
      tree: Tree,
      numBreaks: Int,
      nest: Int
  ): Option[NumBlanks] =
    topStatBlankLinesSorted.iterator
      .takeWhile(_.minBreaks <= numBreaks)
      .find { x =>
        x.minNest <= nest && x.maxNest >= nest &&
        x.pattern.forall(_.matcher(tree.productPrefix).find())
      }
      .flatMap(_.blanks)

  private def getBeforeOpenParen(bop: BeforeOpenParen): SourceHints =
    Option(bop.src).getOrElse(source)
  def getBeforeOpenParenCallSite: Option[SourceHints] =
    beforeOpenParenCallSite.map(getBeforeOpenParen)
  def getBeforeOpenParenDefnSite: Option[SourceHints] =
    beforeOpenParenDefnSite.map(getBeforeOpenParen)
  def isBeforeOpenParenCallSite: Boolean = beforeOpenParenCallSite.isDefined
  def isBeforeOpenParenDefnSite: Boolean = beforeOpenParenDefnSite.isDefined
}

object Newlines {
  implicit lazy val surface: Surface[Newlines] = generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[Newlines] =
    generic.deriveCodecEx(Newlines()).noTypos

  sealed abstract class IgnoreSourceSplit {
    val ignoreSourceSplit: Boolean
    @inline
    final def in(hints: IgnoreSourceSplit*): Boolean = hints.contains(this)
  }

  sealed abstract class SourceHints(val ignoreSourceSplit: Boolean)
      extends IgnoreSourceSplit
  // the classic handler of source newlines
  case object classic extends SourceHints(ignoreSourceSplit = false)
  // try to keep newlines
  case object keep extends SourceHints(ignoreSourceSplit = false)
  // try to fold newlines into spaces (but not semicolons)
  case object fold extends SourceHints(ignoreSourceSplit = true)
  // try to turn spaces and semicolons into newlines
  case object unfold extends SourceHints(ignoreSourceSplit = true)

  object SourceHints {
    // NB: don't allow specifying classic, only by default
    implicit val codec: ConfCodecEx[SourceHints] =
      ReaderUtil.oneOfCustom[SourceHints](keep, fold, unfold) {
        case Conf.Bool(true) => Configured.Ok(unfold)
        case Conf.Bool(false) => Configured.Ok(fold)
      }
  }

  sealed abstract class AfterInfix
  object AfterInfix {
    case object keep extends AfterInfix
    case object some extends AfterInfix
    case object many extends AfterInfix

    implicit val reader: ConfCodecEx[AfterInfix] =
      ReaderUtil.oneOf[AfterInfix](keep, some, many)
  }

  sealed abstract class BeforeAfter
  case object before extends BeforeAfter
  case object after extends BeforeAfter

  implicit val beforeAfterReader: ConfCodecEx[BeforeAfter] =
    ReaderUtil.oneOf[BeforeAfter](before, after)

  sealed abstract class AvoidForSimpleOverflow
  object AvoidForSimpleOverflow {
    case object punct extends AvoidForSimpleOverflow
    case object tooLong extends AvoidForSimpleOverflow
    case object slc extends AvoidForSimpleOverflow
    implicit val codec: ConfCodecEx[AvoidForSimpleOverflow] =
      ReaderUtil.oneOf[AvoidForSimpleOverflow](punct, tooLong, slc)
  }

  sealed abstract class InInterpolation
  object InInterpolation {
    case object allow extends InInterpolation
    case object avoid extends InInterpolation
    case object oneline extends InInterpolation
    implicit val codec: ConfCodecEx[InInterpolation] =
      ReaderUtil.oneOf[InInterpolation](allow, avoid, oneline)
  }

  sealed abstract class AfterCurlyLambdaParams
  object AfterCurlyLambdaParams {
    case object preserve extends AfterCurlyLambdaParams
    case object always extends AfterCurlyLambdaParams
    case object never extends AfterCurlyLambdaParams
    case object squash extends AfterCurlyLambdaParams
    implicit val codec: ConfCodecEx[AfterCurlyLambdaParams] =
      ReaderUtil.oneOfCustom[AfterCurlyLambdaParams](
        preserve,
        always,
        never,
        squash
      ) { case Conf.Str("keep") =>
        Configured.Ok(preserve)
      }
  }

  sealed abstract class BeforeCurlyLambdaParams
  object BeforeCurlyLambdaParams {
    case object always extends BeforeCurlyLambdaParams
    case object never extends BeforeCurlyLambdaParams
    case object multiline extends BeforeCurlyLambdaParams
    case object multilineWithCaseOnly extends BeforeCurlyLambdaParams
    implicit val codec: ConfCodecEx[BeforeCurlyLambdaParams] =
      ReaderUtil.oneOfCustom[BeforeCurlyLambdaParams](
        never,
        always,
        multiline,
        multilineWithCaseOnly
      ) {
        case Conf.Bool(true) => Configured.Ok(always)
        case Conf.Bool(false) => Configured.Ok(never)
      }
  }

  sealed abstract class ForceBeforeMultilineAssign {
    def apply(tree: Tree): Boolean
  }

  object ForceBeforeMultilineAssign {

    implicit val codec: ConfCodecEx[ForceBeforeMultilineAssign] =
      ReaderUtil.oneOf[ForceBeforeMultilineAssign](
        never,
        any,
        `def`,
        anyMember,
        topMember
      )

    case object never extends ForceBeforeMultilineAssign {
      def apply(tree: Tree): Boolean = false
    }
    case object any extends ForceBeforeMultilineAssign {
      def apply(tree: Tree): Boolean = true
    }
    case object `def` extends ForceBeforeMultilineAssign {
      def apply(tree: Tree): Boolean = tree match {
        case _: Tree.WithParamClauses with Stat.WithMods => true
        case _ => false
      }
    }
    case object anyMember extends ForceBeforeMultilineAssign {
      def apply(tree: Tree): Boolean = tree.parent.exists(_.is[Template])
    }
    case object topMember extends ForceBeforeMultilineAssign {
      def apply(tree: Tree): Boolean = {
        // find the first invalid tree
        val nonMemberTree = TreeOps.findTreeWithParentEx(tree) {
          case t: Template => t.parent
          case _: Pkg => None // all trees valid, no need to go further
          case _ => Some(null) // reached invalid parent, no need to go further
        }
        nonMemberTree.isEmpty
      }
    }

  }

  /** @param before
    *   number of blanks to use before the statement
    * @param after
    *   number of blanks to use after the statement
    */
  case class NumBlanks(
      before: Int = 0,
      after: Int = 0,
      beforeAll: Option[Int] = None,
      afterAll: Option[Int] = None,
      beforeEndMarker: Int = 0
  ) {
    def isEmpty: Boolean = before == 0 && after == 0
  }
  object NumBlanks {
    implicit val surface = generic.deriveSurface[NumBlanks]
    implicit val encoder = generic.deriveEncoder[NumBlanks]
    implicit val decoder = {
      val base = generic.deriveDecoderEx(NumBlanks()).noTypos
      ConfDecoderEx.from[NumBlanks] {
        case (_, Conf.Num(num)) if num.isWhole =>
          val cnt = num.toInt
          Configured.Ok(NumBlanks(before = cnt, after = cnt))
        case (state, conf) => base.read(state, conf)
      }
    }
  }

  /** @param regex
    *   Regular expression to match against the statement type
    * @param minBreaks
    *   Minimum span (number of line breaks between first and last line) to
    *   start forcing blank lines.
    * @param minNest
    *   Minimum amount of nesting (indentation level) of a statement
    * @param maxNest
    *   Maximum amount of nesting (indentation level) of a statement
    */
  case class TopStatBlanks(
      regex: Option[String] = None,
      minBreaks: Int = 1,
      blanks: Option[NumBlanks] = None,
      minNest: Int = 0,
      maxNest: Int = Int.MaxValue
  ) {
    lazy val pattern = regex.map(_.r.pattern)
  }
  object TopStatBlanks {
    implicit val surface = generic.deriveSurface[TopStatBlanks]
    implicit val codec: ConfCodecEx[TopStatBlanks] =
      generic.deriveCodecEx(TopStatBlanks()).noTypos
  }

  case class BeforeOpenParen(src: SourceHints = null)
  object BeforeOpenParen {
    implicit val encoder: ConfEncoder[BeforeOpenParen] =
      SourceHints.codec.contramap(_.src)
    implicit val decoder: ConfDecoderEx[BeforeOpenParen] =
      ConfDecoderEx.from {
        case (_, Conf.Str("source")) => Configured.Ok(BeforeOpenParen())
        case (_, _: Conf.Bool) =>
          Configured.error("beforeOpenParen can't be bool")
        case (_, conf) =>
          SourceHints.codec.read(None, conf).map(BeforeOpenParen.apply)
      }
  }

}
