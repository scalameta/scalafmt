package org.scalafmt.config

import metaconfig.ConfigReader

/**
  *
  * @param beforeContextBoundColon formats [A: T] as [A : T]
  * @param afterTripleEquals If true, formats ===( as === (
  * @param inImportCurlyBraces
  *   If true, formats `import a.b.{ c, d }`.
  *   If false, formats `import a.b.{c, d}`.
  * @param beforeSeqWildcard
  * @param neverAroundInfixTypes
  *   If ["##"] is specified as operator then
  *   formats `Generic[Foo] ## Repr` as `Generic[Foo]##Repr`.
  */
@ConfigReader
case class Spaces(
    beforeContextBoundColon: Boolean = false,
    afterTripleEquals: Boolean = false,
    inImportCurlyBraces: Boolean = false,
    beforeSeqWildcard: Boolean = false,
    neverAroundInfixTypes: Seq[String] = Nil
)
