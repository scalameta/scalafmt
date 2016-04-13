package org.scalafmt

case class AlignToken(code: String, owner: String)

object AlignToken {
  // TODO(olafur) Matching against class name is flaky.
  val applyInfix = "Term.ApplyInfix"

  val default = Set(
      AlignToken("extends", "Defn.(Class|Trait|Object)"),
      AlignToken("//", ".*"),
      AlignToken("{", "Template"),
      AlignToken("}", "Template"),
      AlignToken("%", applyInfix),
      AlignToken("%%", applyInfix),
      AlignToken("=>", "Case"),
      AlignToken("->", applyInfix),
      AlignToken("=", "Defn.(Va(l|r)|Def|Type)")
  )
}
