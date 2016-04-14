package org.scalafmt

/**
  * Configuration option for aligning tokens.
  *
  * @param code string literal value of the token to align by.
  * @param owner regexp for class name of scala.meta.Tree "owner" of [[code]].
  */
case class AlignToken(code: String, owner: String)

object AlignToken {
  val applyInfix = "Term.ApplyInfix"

  val default = Set(
      AlignToken("extends", "Defn.(Class|Trait|Object)"),
      AlignToken("//", ".*"),
      AlignToken("{", "Template"),
      AlignToken("}", "Template"),
      AlignToken("%", applyInfix),
      AlignToken("%%", applyInfix),
      AlignToken("=>", "Case"),
      AlignToken("⇒", "Case"),
      AlignToken("<-", "Enumerator.Generator"),
      AlignToken("←", "Enumerator.Generator"),
      AlignToken("->", applyInfix),
      AlignToken("→", applyInfix),
      AlignToken("=", "(Enumerator.Val|Defn.(Va(l|r)|Def|Type))")
  )
}
