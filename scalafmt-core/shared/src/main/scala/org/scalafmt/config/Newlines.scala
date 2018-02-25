package org.scalafmt.config

import metaconfig._

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
  * @param afterImplicitKWInVerticalMultiline  If true, add a newline after an implicit keyword in function and
  *                                   class definitions (only for verticalMultiline style)
  * {{{
  *   // newlines.afterImplicitKWInVerticalMultiline = true
  *   def format(
  *     code: String,
  *     age: Int
  *   )(implicit
  *     ev: Parser,
  *     c: Context
  *   ): String
  *   // newlines.afterImplicitKWInVerticalMultiline = false
  *   def format(
  *     code: String,
  *     age: Int
  *   )(implicit ev: Parser,
  *     c: Context
  *   ): String
  * }}}
  * @param beforeImplicitKWInVerticalMultiline If true, add a newline before an implicit keyword in function and
  *                                   class definitions (only for verticalMultiline style)
  * {{{
  *   // newlines.afterImplicitKWInVerticalMultiline = true
  *   // newlines.beforeImplicitKWInVerticalMultiline = true
  *   def format(
  *     code: String,
  *     age: Int
  *   )(
  *     implicit
  *     ev: Parser,
  *     c: Context
  *   ): String
  *   // newlines.afterImplicitKWInVerticalMultiline = true
  *   // newlines.beforeImplicitKWInVerticalMultiline = false
  *   def format(
  *     code: String,
  *     age: Int
  *   )(implicit
  *     ev: Parser,
  *     c: Context
  *   ): String
  * }}}
  * @param alwaysBeforeElseAfterCurlyIf if true, add a new line between the end of a curly if and the following else.
  *   For example
  *   if(someCond) {
  *     // ...
  *   }
  *   else //...
  */
@DeriveConfDecoder
case class Newlines(
    neverInResultType: Boolean = false,
    neverBeforeJsNative: Boolean = false,
    sometimesBeforeColonInMethodReturnType: Boolean = true,
    penalizeSingleSelectMultiArgList: Boolean = true,
    alwaysBeforeCurlyBraceLambdaParams: Boolean = false,
    alwaysBeforeTopLevelStatements: Boolean = false,
    afterCurlyLambda: NewlineCurlyLambda = NewlineCurlyLambda.never,
    afterImplicitKWInVerticalMultiline: Boolean = false,
    beforeImplicitKWInVerticalMultiline: Boolean = false,
    alwaysBeforeElseAfterCurlyIf: Boolean = false
)

sealed abstract class NewlineCurlyLambda

object NewlineCurlyLambda {

  case object preserve extends NewlineCurlyLambda
  case object always extends NewlineCurlyLambda
  case object never extends NewlineCurlyLambda

  implicit val newlineCurlyLambdaReader: ConfDecoder[NewlineCurlyLambda] =
    ReaderUtil.oneOf[NewlineCurlyLambda](preserve, always, never)
}
