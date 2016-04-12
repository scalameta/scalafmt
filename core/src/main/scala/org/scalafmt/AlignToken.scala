package org.scalafmt

import scala.util.matching.Regex

case class AlignToken(code: String, owner: Regex)

object AlignToken {
  // TODO(olafur) Matching against class name is flaky.
  val applyInfix = ".*Term\\$ApplyInfix".r
  val alignComments = AlignToken("//", ".*".r)
  val alignCaseArrow = AlignToken("=>", ".*Case".r)
  val alignTupleArrow = AlignToken("->", applyInfix)
  val alignAssignment = AlignToken("=", ".*(Va(l|r)|Def)".r)

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
