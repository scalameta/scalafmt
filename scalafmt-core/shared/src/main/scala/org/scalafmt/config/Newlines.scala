package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

/**
  * @param penalizeSingleSelectMultiArgList
  *   If true, adds a penalty to newlines before a dot starting a select
  *   chain of length one and argument list. The penalty matches the number
  *   of arguments to the select chain application.
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
  *   If false, matches pre-v0.5 behavior. Note. this option may be
  *   removed in a future release.
  * @param neverBeforeJsNative If true, a newline will never be placed in
  *                                 front of js.native.
  * @param sometimesBeforeColonInMethodReturnType If true, scalafmt
  *                                               may choose to put a newline
  *                                               before colon : at defs.
  * @param alwaysBeforeCurlyBraceLambdaParams
  *   If true, puts a newline after the open brace
  *   and the parameters list of an anonymous function.
  *   For example
  *   something.map {
  *     n =>
  *       consume(n)
  *   }
  * @param afterCurlyLambda
  *   If `never` (default), it will remove any extra lines below curly lambdas
  *   {{{
  *   something.map { x =>
  *
  *     f(x)
  *   }
  *   }}}
  *   will become
  *   {{{
  *   something.map { x =>
  *     f(x)
  *   }
  *   }}}
  *
  *   If `always`, it will always add one empty line (opposite of `never`).
  *   If `preserve`, and there isn't an empty line, it will keep it as it is.
  *   If there is one or more empty lines, it will place a single empty line.
  * @param alwaysBeforeElseAfterCurlyIf if true, add a new line between the end of a curly if and the following else.
  *   For example
  *   if(someCond) {
  *     // ...
  *   }
  *   else //...
  * @param alwaysBeforeMultilineDef
  *   If true, add a newline before the body of a multiline def without
  *   curly braces. See #1126 for discussion.
  *   For example,
  *   {{{
  *     // newlines.alwaysBeforeMultilineDef = false
  *     def foo(bar: Bar): Foo = bar
  *       .flatMap(f)
  *       .map(g)
  *
  *     // newlines.alwaysBeforeMultilineDef = true
  *     def foo(bar: Bar): Foo =
  *       bar
  *         .flatMap(f)
  *         .map(g)
  *   }}}
  * @param avoidAfterYield
  *   If false (legacy behavior), inserts unconditional line break after `yield`
  *   if the yield body doesn't fit on a single line.
  *   For example,
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
  *   If `classic` (default), the old mixed behaviour will be used
  *   If `keep`, try to keep source newlines
  *   If `fold`, ignore source and try to remove line breaks
  *   If `unfold`, ignore source and try to break lines
  */
case class Newlines(
    source: Newlines.SourceHints = Newlines.classic,
    neverInResultType: Boolean = false,
    neverBeforeJsNative: Boolean = false,
    sometimesBeforeColonInMethodReturnType: Boolean = true,
    penalizeSingleSelectMultiArgList: Boolean = true,
    alwaysBeforeCurlyBraceLambdaParams: Boolean = false,
    alwaysBeforeTopLevelStatements: Boolean = false,
    afterCurlyLambda: NewlineCurlyLambda = NewlineCurlyLambda.never,
    afterImplicitParamListModifier: Boolean = false,
    beforeImplicitParamListModifier: Boolean = false,
    alwaysBeforeElseAfterCurlyIf: Boolean = false,
    alwaysBeforeMultilineDef: Boolean = true,
    avoidAfterYield: Boolean = true
) {
  val reader: ConfDecoder[Newlines] = generic.deriveDecoder(this).noTypos
  if (source != Newlines.classic) {
    Newlines.warnSourceIsExperimental
    require(
      source == Newlines.keep || afterCurlyLambda != NewlineCurlyLambda.preserve,
      s"newlines: can't use source=${source} and afterCurlyLambda=$afterCurlyLambda"
    )
  }

  @inline
  def sourceIs(hint: Newlines.SourceHints): Boolean =
    hint eq source

  @inline
  def sourceIn(hints: Newlines.SourceHints*): Boolean =
    hints.contains(source)

}

object Newlines {
  implicit lazy val surface: Surface[Newlines] = generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[Newlines] = generic.deriveEncoder

  sealed abstract class SourceHints

  // the classic handler of source newlines
  case object classic extends SourceHints

  // try to keep newlines
  case object keep extends SourceHints

  // try to fold newlines into spaces (but not semicolons)
  case object fold extends SourceHints

  // try to turn spaces and semicolons into newlines
  case object unfold extends SourceHints

  implicit val sourceHintsReader: ConfCodec[SourceHints] =
    ReaderUtil.oneOf[SourceHints](keep, fold, unfold)

  private lazy val warnSourceIsExperimental: Unit =
    Console.err.println(
      "newlines.source is experimental, will change ignoring edition setting"
    )
}

sealed abstract class NewlineCurlyLambda

object NewlineCurlyLambda {

  case object preserve extends NewlineCurlyLambda
  case object always extends NewlineCurlyLambda
  case object never extends NewlineCurlyLambda

  implicit val newlineCurlyLambdaReader: ConfCodec[NewlineCurlyLambda] =
    ReaderUtil.oneOf[NewlineCurlyLambda](preserve, always, never)
}
