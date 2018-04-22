package org.scalafmt.config

import metaconfig.Configured.Ok
import metaconfig._
import metaconfig.generic.Surface

/**
  * Configuration option for aligning tokens.
  *
  * @param code string literal value of the token to align by.
  * @param owner regexp for class name of scala.meta.Tree "owner" of [[code]].
  */
case class AlignToken(code: String, owner: String) {
  val reader: ConfDecoder[AlignToken] =
    generic.deriveDecoder(this).noTypos
}

object AlignToken {
  implicit val surface: Surface[AlignToken] = generic.deriveSurface[AlignToken]
  val applyInfix = "Term.ApplyInfix"
  val caseArrow = AlignToken("=>", "Case")
  protected[scalafmt] val fallbackAlign = new AlignToken("<empty>", ".*")
  implicit val DefaultAlignTokenDecoder: ConfDecoder[AlignToken] =
    ConfDecoder.instance[AlignToken] {
      case Conf.Str("caseArrow") => Ok(caseArrow)
      case Conf.Str(regex) => Ok(AlignToken(regex, ".*"))
      case els => fallbackAlign.reader.read(els)
    }

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
