package org.scalafmt

import scala.util.matching.Regex

case class AlignToken(code: String, owner: String)

object AlignToken {
  // TODO(olafur) Matching against class name is flaky.
  val applyInfix = "Term.ApplyInfix"
  val alignComments = AlignToken("//", ".*")
  val alignCaseArrow = AlignToken("=>", "Case")
  val alignTupleArrow = AlignToken("->", applyInfix)
  val alignAssignment = AlignToken("=", "Defn.(Va(l|r)|Def)")

  val alignModuleId = Set(
      AlignToken("%", applyInfix),
      AlignToken("%%", applyInfix)
  )

  val default =
    Set(alignComments,
        alignCaseArrow,
        alignTupleArrow,
        alignAssignment) ++ alignModuleId
}
