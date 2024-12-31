package org.scalafmt.config

import org.scalafmt.config.Newlines._
import org.scalafmt.internal._
import org.scalafmt.util.TreeOps

import scala.meta._

import metaconfig._
import metaconfig.annotation._

/** @param penalizeSingleSelectMultiArgList
  *   - If true, adds a penalty to newlines before a dot starting a select chain
  *     of length one and argument list. The penalty matches the number of
  *     arguments to the select chain application.
  *     {{{
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
  *     }}}
  *   - If false, matches pre-v0.5 behavior. Note. this option may be removed in
  *     a future release.
  *
  * @param neverBeforeJsNative
  *   If true, a newline will never be placed in front of js.native.
  *
  * @param sometimesBeforeColonInMethodReturnType
  *   If true, scalafmt may choose to put a newline before colon : at defs.
  *
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
  *
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
  *
  * @param alwaysBeforeElseAfterCurlyIf
  *   if true, add a new line between the end of a curly if and the following
  *   else. For example
  *   {{{
  *   if(someCond) {
  *     // ...
  *   }
  *   else //...
  *   }}}
  *
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
  *
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
  *
  * @param source
  *   Controls how line breaks in the input source are handled
  *   - If `classic` (default), the old mixed behaviour will be used
  *   - If `keep`, try to keep source newlines
  *   - If `fold`, ignore source and try to remove line breaks
  *   - If `unfold`, ignore source and try to break lines
  *
  * @param infix
  *   Controls how line breaks around infix operations are handled
  *
  * @param topLevelStatementBlankLines
  *   Controls blank line before and/or after a top-level statement.
  *
  * @param avoidForSimpleOverflow
  *   - punct: don't force break if overflow is only due to trailing punctuation
  *   - tooLong: don't force break if overflow is due to tokens which are too
  *     long and would likely overflow even after a break
  *
  * @param annotation
  *   - if `newlines.source` is missing or keep:
  *     - if true, will keep existing line breaks around annotations
  *   - if `newlines.source` is fold:
  *     - if true, will break before the entity being annotated
  *     - will not force break between consecutive annotations
  *   - if `newlines.source` is unfold:
  *     - if true, will break between consecutive annotations
  *     - will always break before the entity being annotated
  *
  * @param selfAnnotation
  *   If true, will keep a line break before a self annotation for
  *   `newlines.source=classic/keep`, or force it for `fold/unfold`; see
  *   [[https://github.com/scalameta/scalafmt/issues/938]]
  */
case class Newlines(
    source: SourceHints = Newlines.classic,
    @ExtraName("neverInResultType")
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
    @DeprecatedName(
      "topLevelStatementsMinBreaks",
      "Use newlines.topLevelStatementBlankLines instead",
      "3.0.0",
    )
    private val topLevelStatementsMinBreaks: Int = 1,
    @DeprecatedName(
      "topLevelStatements",
      "Use newlines.topLevelStatementBlankLines instead",
      "3.0.0",
    )
    private val topLevelStatements: Seq[BeforeAfter] = Seq.empty,
    beforeTemplateBodyIfBreakInParentCtors: Boolean = false,
    topLevelBodyIfMinStatements: Seq[BeforeAfter] = Seq.empty,
    topLevelBodyMinStatements: Int = 2,
    @ExtraName("afterCurlyLambda")
    afterCurlyLambdaParams: AfterCurlyLambdaParams =
      AfterCurlyLambdaParams.never,
    @ExtraName("usingParamListModifierForce")
    implicitParamListModifierForce: Seq[BeforeAfter] = Seq.empty,
    @ExtraName("usingParamListModifierPrefer")
    implicitParamListModifierPrefer: Option[BeforeAfter] = None,
    alwaysBeforeElseAfterCurlyIf: Boolean = false,
    forceBeforeAssign: ForceBeforeMultilineAssign =
      ForceBeforeMultilineAssign.never,
    private val forceBeforeMultilineAssign: Option[ForceBeforeMultilineAssign] =
      None,
    private[config] val beforeMultiline: Option[SourceHints] = None,
    @DeprecatedName(
      "beforeMultilineDef",
      "Use newlines.beforeMultiline, newlines.forceBeforeMultilineAssign instead",
      "3.0.0",
    )
    beforeMultilineDef: Option[SourceHints] = None,
    selectChains: SelectChain = SelectChain.default,
    infix: Infix = Infix.default,
    avoidForSimpleOverflow: Seq[AvoidForSimpleOverflow] = Seq.empty,
    inInterpolation: InInterpolation = InInterpolation.allow,
    ignoreInSyntax: Boolean = true,
    avoidAfterYield: Boolean = true,
    configStyle: ConfigStyle = ConfigStyle.default,
    annotation: Boolean = true,
    selfAnnotation: Boolean = true,
) {
  if (
    implicitParamListModifierForce.nonEmpty &&
    implicitParamListModifierPrefer.nonEmpty
  ) throw new ScalafmtConfigException(
    "can't specify both " +
      "implicitParamListModifierForce and implicitParamListModifierPrefer",
  )

  @inline
  def sourceIgnored: Boolean = source.ignoreSourceSplit

  def okSpaceForSource(newlines: Int, forFold: => Boolean = true): Boolean =
    source match {
      case Newlines.fold => forFold
      case Newlines.unfold => false
      case _ => newlines == 0
    }

  @inline
  def fold: Boolean = source eq Newlines.fold
  @inline
  def unfold: Boolean = source eq Newlines.unfold
  @inline
  def keep: Boolean = source eq Newlines.keep
  @inline
  def classic: Boolean = source eq Newlines.classic
  @inline
  def keepBreak(hasBreak: => Boolean): Boolean = keep && hasBreak
  @inline
  def keepBreak(newlines: Int): Boolean = keepBreak(!FT.noBreak(newlines))
  @inline
  def keepBreak(implicit ft: FT): Boolean = keepBreak(ft.hasBreak)

  def checkInfixConfig(termCnt: Int, typeCnt: Int, patCnt: Int)(implicit
      cfg: ScalafmtConfig,
  ): Newlines = copy(infix = infix.checkInfixCounts(termCnt, typeCnt, patCnt))

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
    else !forceBeforeImplicitParamListModifier

  lazy val avoidForSimpleOverflowPunct: Boolean = avoidForSimpleOverflow
    .contains(AvoidForSimpleOverflow.punct)
  lazy val avoidForSimpleOverflowTooLong: Boolean = avoidForSimpleOverflow
    .contains(AvoidForSimpleOverflow.tooLong)
  lazy val avoidForSimpleOverflowSLC: Boolean = avoidForSimpleOverflow
    .contains(AvoidForSimpleOverflow.slc)

  @inline
  def alwaysBeforeCurlyLambdaParams = beforeCurlyLambdaParams eq
    BeforeCurlyLambdaParams.always

  lazy val getBeforeMultiline = beforeMultiline.getOrElse(source)
  lazy val shouldForceBeforeMultilineAssign = forceBeforeMultilineAssign
    .getOrElse {
      val useDef = beforeMultilineDef.contains(Newlines.unfold)
      if (useDef) ForceBeforeMultilineAssign.`def`
      else ForceBeforeMultilineAssign.never
    }

  lazy val getSelectChains = selectChains.style.getOrElse(source)

  private lazy val topStatBlankLinesSorted =
    if (topLevelStatementBlankLines.isEmpty) {
      val nb = NumBlanks(
        if (topLevelStatements.contains(before)) 1 else 0,
        if (topLevelStatements.contains(after)) 1 else 0,
      )
      if (nb.isEmpty) Seq.empty
      else {
        val pattern = Some("^Pkg|^Defn\\.|^Decl\\.")
        Seq(TopStatBlanks(pattern, topLevelStatementsMinBreaks, Some(nb)))
      }
    } else
      /* minBreaks has to come first; since we'll be adding blanks, this could
       * potentially move us into another setting which didn't match before we
       * we added the blanks; the rest are sorted to put more specific first */
      topLevelStatementBlankLines.filter(x => x.minNest <= x.maxNest).sortBy(
        x => (x.minBreaks, x.maxNest, -x.minNest, x.regex.fold(0)(-_.length)),
      )

  @inline
  def hasTopStatBlankLines = topStatBlankLinesSorted.nonEmpty

  def getTopStatBlankLines(
      tree: Tree,
  )(params: TopStatBlanksParams): Option[NumBlanks] = {
    val prefix = tree.productPrefix
    topStatBlankLinesSorted.iterator.takeWhile(_.minBreaks <= params.numBreaks)
      .find(x => x.checkParams(params, prefix)).flatMap(_.blanks)
  }

  private def getBeforeOpenParen(bop: BeforeOpenParen): SourceHints =
    Option(bop.src).getOrElse(source)
  def getBeforeOpenParenCallSite: Option[SourceHints] = beforeOpenParenCallSite
    .map(getBeforeOpenParen)
  def getBeforeOpenParenDefnSite: Option[SourceHints] = beforeOpenParenDefnSite
    .map(getBeforeOpenParen)
  def isBeforeOpenParenCallSite: Boolean = beforeOpenParenCallSite.isDefined
  def isBeforeOpenParenDefnSite: Boolean = beforeOpenParenDefnSite.isDefined

  private[scalafmt] lazy val encloseSelectChains = selectChains.enclose
    .getOrElse(!classic)

}

object Newlines {
  implicit lazy val surface: generic.Surface[Newlines] = generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[Newlines] = generic
    .deriveCodecEx(Newlines()).noTypos.withSectionRenames(
      // deprecated since v3.0.4
      SectionRename("configStyleCallSite", "configStyle.callSite"),
      // deprecated since v3.0.4
      SectionRename("configStyleDefnSite", "configStyle.defnSite"),
      // deprecated since v3.0.0
      SectionRename { case Conf.Bool(value) =>
        Conf.Str(if (value) "def" else "never")
      }("alwaysBeforeMultilineDef", "forceBeforeMultilineAssign"),
      // deprecated since v3.8.4
      SectionRename("afterInfix", "infix.termSite.style"),
      SectionRename("afterInfixBreakOnNested", "infix.termSite.breakOnNested"),
      SectionRename(
        "afterInfixMaxCountPerFile",
        "infix.termSite.maxCountPerFile",
      ),
      SectionRename(
        "afterInfixMaxCountPerExprForSome",
        "infix.termSite.maxCountPerExprForSome",
      ),
    )

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
    implicit val codec: ConfCodecEx[SourceHints] = ReaderUtil
      .oneOfCustom[SourceHints](keep, fold, unfold) {
        case Conf.Bool(true) => Configured.Ok(unfold)
        case Conf.Bool(false) => Configured.Ok(fold)
      }
  }

  case class Infix(
      private val termSite: Infix.Site = Infix.Site.default,
      private val typeSite: Option[Infix.Site] = None,
      private val patSite: Option[Infix.Site] = None,
  ) {
    def checkInfixCounts(termCnt: Int, typeInfix: Int, patInfix: Int)(implicit
        cfg: ScalafmtConfig,
    ): Infix = copy(
      termSite = termSite.checkConfig(termCnt)(None),
      typeSite = Some(typeSite.getOrElse(termSite).checkConfig(typeInfix) {
        val useSome = !cfg.newlines.keep && cfg.dialect.useInfixTypePrecedence
        if (useSome) Some(Infix.some) else None
      }),
      patSite = Some(patSite.getOrElse(termSite).checkConfig(patInfix)(None)),
    )

    def get(tree: Tree): Infix.Site = tree match {
      case _: Type => typeSite.getOrElse(termSite)
      case _: Pat => patSite.getOrElse(termSite)
      case _ => termSite
    }

    def keep(tree: Tree): Boolean = get(tree).isKeep
  }

  object Infix {
    private[Newlines] val default = Infix()
    implicit lazy val surface: generic.Surface[Infix] = generic.deriveSurface
    implicit lazy val codec: ConfCodecEx[Infix] = generic.deriveCodecEx(default)
      .noTypos

    sealed abstract class Style
    case object keep extends Style
    case object some extends Style
    case object many extends Style
    implicit val styleReader: ConfCodecEx[Style] = ReaderUtil
      .oneOf[Style](keep, some, many)

    /** @param style
      *   Controls how line breaks around infix operations are handled
      *   - If `keep` (default for source=classic,keep), preserve existing
      *   - If `some` (default for source=fold), break after some infix ops
      *   - If `many` (default for source=unfold), break after many infix ops
      * @param breakOnNested
      *   Force breaks around nested (enclosed in parentheses) expressions
      * @param maxCountPerFile
      *   Switch to `keep` for a given file if the total number of infix
      *   operations in that file exceeds this value
      * @param maxCountPerExprForSome
      *   Switch to `many` for a given expression (possibly nested) if the
      *   number of operations in that expression exceeds this value AND
      *   `afterInfix` had been set to `some`.
      */
    case class Site(
        style: Style = null,
        breakOnNested: Boolean = false,
        maxCountPerFile: Int = 500,
        maxCountPerExprForSome: Int = 10,
    ) {
      def checkConfig(
          infixCount: Int,
      )(orElseStyle: => Option[Style])(implicit cfg: ScalafmtConfig): Site =
        if (maxCountPerFile < infixCount) copy(style = keep)
        else if (style eq null) Infix.defaultStyle(cfg.newlines.source)
          .orElse(orElseStyle).fold(this)(x => copy(style = x))
        else this
      def isKeep: Boolean = (style eq keep) || (style eq null)
    }
    object Site {
      private[Infix] val default = Site()
      implicit lazy val surface: generic.Surface[Site] = generic.deriveSurface
      implicit lazy val codec: ConfCodecEx[Site] = generic.deriveCodecEx(default)
        .noTypos
    }

    def defaultStyle(source: SourceHints): Option[Style] = source match {
      case Newlines.unfold => Some(many)
      case Newlines.fold => Some(some)
      case _ => None
    }
  }

  sealed abstract class BeforeAfter
  case object before extends BeforeAfter
  case object after extends BeforeAfter

  implicit val beforeAfterReader: ConfCodecEx[BeforeAfter] = ReaderUtil
    .oneOf[BeforeAfter](before, after)

  sealed abstract class AvoidForSimpleOverflow
  object AvoidForSimpleOverflow {
    case object punct extends AvoidForSimpleOverflow
    case object tooLong extends AvoidForSimpleOverflow
    case object slc extends AvoidForSimpleOverflow

    val all: Seq[sourcecode.Text[AvoidForSimpleOverflow]] =
      Seq(punct, tooLong, slc)

    implicit val codec: ConfCodecEx[AvoidForSimpleOverflow] = ReaderUtil
      .oneOf[AvoidForSimpleOverflow](all: _*)

    implicit val seqDecoder: ConfDecoderEx[Seq[AvoidForSimpleOverflow]] =
      ConfDecoderEx.fromPartial[Seq[AvoidForSimpleOverflow]]("STR") {
        case (_, Conf.Str("all")) => Configured.ok(all.map(_.value))
      }.orElse(ConfDecoderExT.canBuildSeq[AvoidForSimpleOverflow, Seq])
  }

  sealed abstract class InInterpolation
  object InInterpolation {
    case object allow extends InInterpolation
    case object avoid extends InInterpolation
    case object oneline extends InInterpolation
    implicit val codec: ConfCodecEx[InInterpolation] = ReaderUtil
      .oneOf[InInterpolation](allow, avoid, oneline)
  }

  sealed abstract class AfterCurlyLambdaParams
  object AfterCurlyLambdaParams {
    case object preserve extends AfterCurlyLambdaParams
    case object always extends AfterCurlyLambdaParams
    case object never extends AfterCurlyLambdaParams
    case object squash extends AfterCurlyLambdaParams
    implicit val codec: ConfCodecEx[AfterCurlyLambdaParams] = ReaderUtil
      .oneOfCustom[AfterCurlyLambdaParams](preserve, always, never, squash) {
        case Conf.Str("keep") => Configured.Ok(preserve)
      }
  }

  sealed abstract class BeforeCurlyLambdaParams
  object BeforeCurlyLambdaParams {
    case object always extends BeforeCurlyLambdaParams
    case object never extends BeforeCurlyLambdaParams
    case object multiline extends BeforeCurlyLambdaParams
    case object multilineWithCaseOnly extends BeforeCurlyLambdaParams
    implicit val codec: ConfCodecEx[BeforeCurlyLambdaParams] = ReaderUtil
      .oneOfCustom[BeforeCurlyLambdaParams](
        never,
        always,
        multiline,
        multilineWithCaseOnly,
      ) {
        case Conf.Bool(true) => Configured.Ok(always)
        case Conf.Bool(false) => Configured.Ok(never)
      }
  }

  sealed abstract class ForceBeforeMultilineAssign {
    def apply(tree: Tree): Boolean
  }

  object ForceBeforeMultilineAssign {

    implicit val codec: ConfCodecEx[ForceBeforeMultilineAssign] = ReaderUtil
      .oneOf[ForceBeforeMultilineAssign](never, any, `def`, anyMember, topMember)

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
      def apply(tree: Tree): Boolean = tree.parent.is[Template.Body]
    }
    case object topMember extends ForceBeforeMultilineAssign {
      def apply(tree: Tree): Boolean = {
        // find the first invalid tree
        val nonMemberTree = TreeOps.findTreeWithParentEx(tree) {
          case t: Template.Body => t.parent.parent
          case _: Pkg.Body => None // all trees valid, no need to go further
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
      beforeEndMarker: Int = 0,
  ) {
    def isEmpty: Boolean = before == 0 && after == 0
  }
  object NumBlanks {
    implicit val surface: generic.Surface[NumBlanks] = generic
      .deriveSurface[NumBlanks]
    implicit val encoder: ConfEncoder[NumBlanks] = generic
      .deriveEncoder[NumBlanks]
    implicit val decoder: ConfDecoderEx[NumBlanks] = {
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
      maxNest: Int = Int.MaxValue,
      minBlankGaps: Int = 0,
      maxBlankGaps: Int = Int.MaxValue,
  ) {
    lazy val pattern = regex.map(_.r.pattern)
    def checkParams(v: TopStatBlanksParams, prefix: String): Boolean =
      checkRange(v.nest, minNest, maxNest) &&
        checkRange(v.blankGaps, minBlankGaps, maxBlankGaps) &&
        pattern.forall(_.matcher(prefix).find())
  }
  object TopStatBlanks {
    implicit val surface: generic.Surface[TopStatBlanks] = generic
      .deriveSurface[TopStatBlanks]
    implicit val codec: ConfCodecEx[TopStatBlanks] = generic
      .deriveCodecEx(TopStatBlanks()).noTypos
  }
  case class TopStatBlanksParams( // what to match against in checkParams above
      numBreaks: Int,
      nest: Int,
      blankGaps: Int,
  )
  private def checkRange(v: Int, min: Int, max: Int) = min <= v && v <= max

  case class BeforeOpenParen(src: SourceHints = null)
  object BeforeOpenParen {
    implicit val encoder: ConfEncoder[BeforeOpenParen] = SourceHints.codec
      .contramap(_.src)
    implicit val decoder: ConfDecoderEx[BeforeOpenParen] = ConfDecoderEx.from {
      case (_, Conf.Str("source")) => Configured.Ok(BeforeOpenParen())
      case (_, _: Conf.Bool) => Configured.error("beforeOpenParen can't be bool")
      case (_, conf) => SourceHints.codec.read(None, conf)
          .map(BeforeOpenParen.apply)
    }
  }

  /** Clauses where there is a newline after opening `(`` and newline before
    * closing `)`. If true, preserves the newlines and keeps one line per
    * argument.
    */
  case class ConfigStyleElement(
      prefer: Boolean = true,
      private val forceIfOptimized: Option[Boolean] = None,
  ) {
    @inline
    def getForceIfOptimized: Boolean = forceIfOptimized.getOrElse(prefer)
  }
  private[config] object ConfigStyleElement {
    private val default = ConfigStyleElement()
    implicit val surface: generic.Surface[ConfigStyleElement] = generic
      .deriveSurface[ConfigStyleElement]
    implicit val codec: ConfCodecEx[ConfigStyleElement] = generic
      .deriveCodecEx(default).noTypos
  }

  case class ConfigStyle(
      callSite: Option[ConfigStyleElement] = None,
      defnSite: Option[ConfigStyleElement] = None,
      bracketCallSite: Option[ConfigStyleElement] = None,
      bracketDefnSite: Option[ConfigStyleElement] = None,
      fallBack: ConfigStyleElement = ConfigStyleElement(),
      @DeprecatedName(
        "beforeComma",
        "Scala supports trailing commas after 2.12.2. Use trailingCommas instead",
        "2.5.0",
      )
      beforeComma: Boolean = false,
  ) {
    def getParenCallSite: ConfigStyleElement = callSite.getOrElse(fallBack)
    def getBracketCallSite: ConfigStyleElement = bracketCallSite
      .getOrElse(getParenCallSite)
    def getCallSite(isBracket: Boolean): ConfigStyleElement =
      if (isBracket) getBracketCallSite else getParenCallSite
    @inline
    def getCallSite(tree: Tree): ConfigStyleElement =
      getCallSite(tree.is[Type.ArgClause])

    def getParenDefnSite: ConfigStyleElement = defnSite.getOrElse(fallBack)
    def getBracketDefnSite: ConfigStyleElement = bracketDefnSite
      .getOrElse(getParenDefnSite)
    def getDefnSite(isBracket: Boolean): ConfigStyleElement =
      if (isBracket) getBracketDefnSite else getParenDefnSite
    @inline
    def getDefnSite(tree: Tree): ConfigStyleElement =
      getDefnSite(tree.is[Type.ArgClause])
  }
  private[config] object ConfigStyle {
    val default = ConfigStyle()
    implicit val surface: generic.Surface[ConfigStyle] = generic
      .deriveSurface[ConfigStyle]
    implicit val codec: ConfCodecEx[ConfigStyle] = generic.deriveCodecEx(default)
      .noTypos
  }

  /** @param classicKeepAfterFirstBreak
    *   NB: failure unless newlines.source=classic If true, then the user can
    *   opt out of line breaks inside select chains.
    *   {{{
    *     // original
    *     foo
    *       .map(_ + 1).map(_ + 1)
    *       .filter(_ > 2)
    *     // if true
    *     foo
    *       .map(_ + 1).map(_ + 1)
    *       .filter(_ > 2)
    *     // if false
    *     foo
    *       .map(_ + 1)
    *       .map(_ + 1)
    *       .filter(_ > 2)
    *   }}}
    *
    * @param classicKeepFirst
    *   NB: ignored unless newlines.source=classic If true, keeps the line break
    *   before a dot if it already exists.
    *   {{{
    *     // original
    *     foo
    *       .map(_ + 1)
    *       .filter( > 2)
    *     // if true
    *     foo
    *       .map(_ + 1)
    *       .filter( > 2)
    *     // if false
    *     foo.map(_ + 1).filter( > 2)
    *   }}}
    *
    * @param encloseIfClassic
    *   NB: ignored unless newlines.source=classic. Controls what happens if a
    *   chain enclosed in parentheses is followed by additional selects. Those
    *   additional selects will be considered part of the enclosed chain if and
    *   only if this flag is false.
    *   {{{
    *     // original
    *     (foo.map(_ + 1).map(_ + 1))
    *       .filter(_ > 2)
    *     // if true
    *     (foo.map(_ + 1).map(_ + 1))
    *       .filter(_ > 2)
    *     // if false
    *     (foo
    *       .map(_ + 1)
    *       .map(_ + 1))
    *       .filter(_ > 2)
    *   }}}
    */

  case class SelectChain(
      style: Option[SourceHints] = None,
      enclose: Option[Boolean] = None,
      // classic-only parameters
      classicKeepFirst: Boolean = true,
      classicKeepAfterFirstBreak: Boolean = false,
      classicCanStartWithBraceApply: Boolean = true,
      classicCanStartWithoutApply: Boolean = false,
  )
  private[config] object SelectChain {
    val default = SelectChain()
    implicit val surface: generic.Surface[SelectChain] = generic
      .deriveSurface[SelectChain]
    implicit val encoder: ConfEncoder[SelectChain] = generic.deriveEncoder
    implicit val decoder: ConfDecoderEx[SelectChain] = {
      val base = generic.deriveDecoderEx(default).noTypos
      ConfDecoderEx.from {
        case (_, conf: Conf.Str) => SourceHints.codec.read(None, conf)
            .map(x => SelectChain(style = Some(x)))
        case (state, conf) => base.read(state, conf)
      }
    }
  }

}
