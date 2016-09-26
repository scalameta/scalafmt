package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * Configuration option for aligning tokens.
  *
  * @param code string literal value of the token to align by.
  * @param owner regexp for class name of scala.meta.Tree "owner" of [[code]].
  */
@ConfigReader
case class AlignToken(code: String, owner: String)


object AlignToken {
  val applyInfix = "Term.ApplyInfix"
  val caseArrow = AlignToken("=>", "Case")

  val default = Set(
    caseArrow,
    AlignToken("extends", "Defn.(Class|Trait|Object)"),
    AlignToken("//", ".*"),
    AlignToken("{", "Template"),
    AlignToken("}", "Template"),
    AlignToken("%", applyInfix),
    AlignToken("%%", applyInfix),
    AlignToken("%%%", applyInfix),
    AlignToken("⇒", "Case"),
    AlignToken("<-", "Enumerator.Generator"),
    AlignToken("←", "Enumerator.Generator"),
    AlignToken("->", applyInfix),
    AlignToken("→", applyInfix),
    AlignToken("=", "(Enumerator.Val|Defn.(Va(l|r)|Def|Type))")
  )
}
