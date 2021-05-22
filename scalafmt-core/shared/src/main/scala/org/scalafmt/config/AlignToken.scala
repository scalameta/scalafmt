package org.scalafmt.config

import metaconfig.Configured.Ok
import metaconfig._
import metaconfig.generic.Surface

/** Configuration option for aligning tokens.
  *
  * @param code string literal value of the token to align by.
  * @param owner regexp for class name of scala.meta.Tree "owner" of [[code]].
  */
case class AlignToken(code: String, owner: String)

object AlignToken {
  implicit lazy val surface: Surface[AlignToken] =
    generic.deriveSurface[AlignToken]
  implicit lazy val encoder: ConfEncoder[AlignToken] = generic.deriveEncoder
  val applyInfix = "Term.ApplyInfix"
  val caseArrow = AlignToken("=>", "Case")
  protected[scalafmt] val fallbackAlign = new AlignToken("<empty>", ".*")
  implicit val decoder: ConfDecoderEx[AlignToken] = {
    val base = generic.deriveDecoderEx[AlignToken](fallbackAlign).noTypos
    ConfDecoderEx.from {
      case (_, Conf.Str("caseArrow")) => Ok(caseArrow)
      case (_, Conf.Str(regex)) =>
        val owner = default.find(_.code == regex).fold(".*")(_.owner)
        Ok(AlignToken(regex, owner))
      case (state, conf) => base.read(state, conf)
    }
  }
  val seqDecoder: ConfDecoderEx[Seq[AlignToken]] = implicitly

  val default = Seq(
    caseArrow,
    AlignToken("extends", "Template"),
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
    AlignToken("=", "(Enumerator.Val|Defn.(Va(l|r)|GivenAlias|Def|Type))")
  )
}
