package org.scalafmt.config

import org.scalafmt.util.ParamClauseParent

import java.util.regex.{Pattern => jurPattern}

import metaconfig._

/** Configuration option for aligning tokens.
  *
  * @param code
  *   string literal value of the token to align by.
  * @param owners
  *   array of owner specs.
  */

case class AlignToken(code: String, owners: Seq[AlignToken.Owner] = Seq.empty) {
  def getMatcher: Seq[AlignToken.Matcher] = owners.distinct.map(_.getMatcher)
}

object AlignToken {

  def apply(code: String, owner: String): AlignToken = {
    val owners = Option(owner) match {
      case None => Seq.empty
      case x => Seq(AlignToken.Owner(x))
    }
    AlignToken(code, owners)
  }

  private def pattern(value: String): jurPattern = value.r.pattern

  /** @param regex
    *   regexp for class name of scala.meta.Tree "owner".
    * @param parents
    *   optional regexp for class name of owner's parent.
    */
  case class Owner(
      regex: Option[String] = None,
      parents: Seq[String] = Seq.empty,
  ) {
    def getMatcher: Matcher =
      new Matcher(regex.map(pattern), parents.map(pattern))
  }
  implicit val ownerSurface: generic.Surface[Owner] = generic
    .deriveSurface[Owner]
  implicit val ownerCodec: ConfCodecEx[Owner] = generic.deriveCodecEx(Owner())

  implicit lazy val surface: generic.Surface[AlignToken] = generic
    .deriveSurface[AlignToken]
  implicit lazy val encoder: ConfEncoder[AlignToken] = generic.deriveEncoder
  val applyInfix = "Term.ApplyInfix"
  val caseArrow = AlignToken("=>", "Case")
  protected[scalafmt] val fallbackAlign = new AlignToken("<empty>")
  implicit val decoder: ConfDecoderEx[AlignToken] = {
    val base = generic.deriveDecoderEx[AlignToken](fallbackAlign).noTypos
      .withSectionRenames(
        // deprecated since v3.0.0
        annotation.SectionRename { case x: Conf.Str =>
          Conf.Lst(Conf.Obj("regex" -> x))
        }("owner", "owners"),
      )
    ConfDecoderEx.from {
      case (_, Conf.Str("caseArrow")) => Configured.Ok(caseArrow)
      case (_, Conf.Str(regex)) => Configured
          .Ok(default.find(_.code == regex).getOrElse(AlignToken(regex)))
      case (state, conf) => base.read(state, conf)
    }
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

  class Matcher(val owner: Option[jurPattern], val parents: Seq[jurPattern]) {
    def matches(tree: meta.Tree): Boolean = owner.forall(check(tree)) &&
      (parents.isEmpty || tree.parent.exists(p =>
        parents.forall(check(p)) ||
          (p match {
            case ParamClauseParent(pp) => parents.forall(check(pp))
            case _: meta.Member.SyntaxValuesClause => p.parent
                .exists(pp => parents.forall(check(pp)))
            case _ => false
          }),
      ))
  }

  @inline
  private def check(tree: meta.Tree)(pattern: jurPattern): Boolean = pattern
    .matcher(tree.productPrefix).find()

}
