package org.scalafmt.config

import org.scalafmt.util.TokenOps

import scala.meta.tokens.{Token => T}

import metaconfig._

/** @param beforeContextBoundColon
  *   formats [A: T] as [A : T]
  * @param afterTripleEquals
  *   If true, formats ===( as === (
  * @param inImportCurlyBraces
  *   - If true, formats
  *     {{{
  *     import a.b.{ c, d}
  *     }}}
  *   - If false, formats
  *     {{{
  *     import a.b.{c, d}
  *     }}}
  * @param inInterpolatedStringCurlyBraces
  *   If true, formats
  *   {{{
  *     s"\${var1} \${var2.sub}"
  *   }}}
  *   as
  *   {{{
  *     s"\${ var1 } \${ var2.sub }"
  *   }}}
  * @param inParentheses
  *   If true, formats `foo(a, b)` as `foo( a, b )`.
  * @param neverAroundInfixTypes
  *   If ["##"] is specified as operator then formats {{{Generic[Foo] ## Repr}}}
  *   as {{{Generic[Foo]##Repr}}}.
  * @param afterKeywordBeforeParen
  *   if false, does not add a space between a keyword and a parenthesis. For
  *   example:
  *   {{{
  *     if(a) println("HELLO!")
  *     while(a) println("HELLO!")
  *   }}}
  * @param inByNameTypes
  *   If false, removes space in by-name parameter: {{{def foo(a: =>A)}}}
  * @param afterSymbolicDefs
  *   If true, adds a single space after an operator method. For example:
  *   {{{def <=> [T](that: T): Boolean}}}
  */
case class Spaces(
    beforeContextBoundColon: Spaces.BeforeContextBound =
      Spaces.BeforeContextBound.Never,
    beforeApplyArgInParens: Spaces.BeforeArgInParens =
      Spaces.BeforeArgInParens.Never,
    beforeInfixArgInParens: Spaces.BeforeArgInParens =
      Spaces.BeforeArgInParens.Always,
    afterTripleEquals: Boolean = false,
    inImportCurlyBraces: Boolean = false,
    inInterpolatedStringCurlyBraces: Boolean = false,
    inParentheses: Boolean = false,
    neverAroundInfixTypes: Seq[String] = Nil,
    aroundSymbolicInfixOperators: Option[FilterMatcher] = None,
    afterKeywordBeforeParen: Boolean = true,
    inByNameTypes: Boolean = true,
    afterSymbolicDefs: Boolean = false,
    private val afterColonInMatchPattern: Spaces.AfterColonInMatchPattern =
      Spaces.AfterColonInMatchPattern.Always,
) {
  def isSpaceAfterKeyword(tokenAfter: T): Boolean = afterKeywordBeforeParen ||
    !tokenAfter.is[T.LeftParen]

  def notAfterColon(owner: meta.Tree): Boolean = owner match {
    case x: meta.Pat.Typed => afterColonInMatchPattern match {
        case Spaces.AfterColonInMatchPattern.Never => true
        case Spaces.AfterColonInMatchPattern.Always => false
        case Spaces.AfterColonInMatchPattern.NoAlternatives => x.parent
            .is[meta.Pat.Alternative]
      }
    case _ => false
  }

}

object Spaces {
  implicit lazy val surface: generic.Surface[Spaces] = generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[Spaces] = generic.deriveCodecEx(Spaces())
    .noTypos

  sealed abstract class BeforeContextBound
  object BeforeContextBound {
    implicit val codec: ConfCodecEx[BeforeContextBound] = ReaderUtil
      .oneOfCustom[BeforeContextBound](Always, Never, IfMultipleBounds) {
        case Conf.Bool(true) => Configured.ok(Always)
        case Conf.Bool(false) => Configured.ok(Never)
      }

    case object Always extends BeforeContextBound
    case object Never extends BeforeContextBound
    case object IfMultipleBounds extends BeforeContextBound
  }

  sealed abstract class AfterColonInMatchPattern
  object AfterColonInMatchPattern {
    implicit val codec: ConfCodecEx[AfterColonInMatchPattern] = ReaderUtil
      .oneOf[AfterColonInMatchPattern](Always, Never, NoAlternatives)
    case object Always extends AfterColonInMatchPattern
    case object Never extends AfterColonInMatchPattern
    case object NoAlternatives extends AfterColonInMatchPattern
  }

  sealed abstract class BeforeArgInParens {
    def apply(name: => String): Boolean
  }
  private object BeforeArgInParens {
    implicit val codec: ConfCodecEx[BeforeArgInParens] = ReaderUtil
      .oneOfCustom[BeforeArgInParens](Never, Always, AfterSymbolic) {
        case Conf.Bool(true) => Configured.ok(Always)
        case Conf.Bool(false) => Configured.ok(Never)
      }

    case object Never extends BeforeArgInParens {
      def apply(name: => String): Boolean = false
    }
    case object Always extends BeforeArgInParens {
      def apply(name: => String): Boolean = true
    }
    case object AfterSymbolic extends BeforeArgInParens {
      def apply(name: => String): Boolean = TokenOps.isSymbolicName(name)
    }
  }

}
