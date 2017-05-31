package org.scalafmt.config

import metaconfig._
import org.scalafmt.config.SpaceBeforeContextBound.Never

/**
  *
  * @param beforeContextBoundColon formats [A: T] as [A : T]
  * @param afterTripleEquals If true, formats ===( as === (
  * @param inImportCurlyBraces
  *   If true, formats `import a.b.{ c, d }`.
  *   If false, formats `import a.b.{c, d}`.
  * @param inParentheses If true, formats `foo(a, b)` as `foo( a, b )`.
  * @param neverAroundInfixTypes
  *   If ["##"] is specified as operator then
  *   formats `Generic[Foo] ## Repr` as `Generic[Foo]##Repr`.
  */
@DeriveConfDecoder
case class Spaces(
    beforeContextBoundColon: SpaceBeforeContextBound = Never,
    afterTripleEquals: Boolean = false,
    inImportCurlyBraces: Boolean = false,
    inParentheses: Boolean = false,
    neverAroundInfixTypes: Seq[String] = Nil
)
