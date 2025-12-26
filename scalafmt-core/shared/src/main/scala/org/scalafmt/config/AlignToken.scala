package org.scalafmt.config

import metaconfig._

/** Configuration option for aligning tokens.
  *
  * @param code
  *   string literal value of the token to align by.
  * @param owners
  *   array of owner specs.
  */

case class AlignToken(code: String, owners: Seq[TreePattern] = Seq.empty) {
  def getMatcher: Seq[TreePattern.Matcher] = owners.distinct.map(_.getMatcher)
}

object AlignToken {

  def apply(code: String, owner: String): AlignToken = {
    val owners = Option(owner) match {
      case None => Seq.empty
      case x => Seq(TreePattern(x))
    }
    AlignToken(code, owners)
  }

  implicit lazy val surface: generic.Surface[AlignToken] = generic
    .deriveSurface[AlignToken]
  implicit lazy val encoder: ConfEncoder[AlignToken] = generic.deriveEncoder
  val applyInfix = "Term.ApplyInfix"
  val caseArrow = AlignToken("=>", "Case")
  protected[scalafmt] val fallbackAlign = new AlignToken("<empty>")
  implicit val decoder: ConfDecoderEx[AlignToken] = generic
    .deriveDecoderEx[AlignToken](fallbackAlign).noTypos.withSectionRenames(
      // deprecated since v3.0.0
      annotation.SectionRename { case x: Conf.Str =>
        Conf.Lst(Conf.Obj("regex" -> x))
      }("owner", "owners"),
    ).except {
      case (_, Conf.Str("caseArrow")) => Some(Configured.Ok(caseArrow))
      case (_, Conf.Str(regex)) => Some(
          Configured.Ok(default.find(_.code == regex).getOrElse(AlignToken(regex))),
        )
      case _ => None
    }
  val seqDecoder: ConfDecoderEx[Seq[AlignToken]] = implicitly

  val default = Seq(
    caseArrow,
    AlignToken("extends", raw"Template|Defn\.EnumCase"),
    AlignToken("//"),
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
    AlignToken(":=", applyInfix),
    AlignToken("=", "(Enumerator.Val|Defn.(Va(l|r)|GivenAlias|Def|Type))"),
  )

}
