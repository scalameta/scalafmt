package org.scalafmt.config

import java.util.regex.{Pattern => jurPattern}

import metaconfig.Configured.Ok
import metaconfig._
import metaconfig.annotation.DeprecatedName
import metaconfig.generic.Surface
import org.scalafmt.util.ParamClauseParent

/** Configuration option for aligning tokens.
  *
  * @param code
  *   string literal value of the token to align by.
  * @param owners
  *   array of owner specs.
  */
case class AlignToken(
    code: String,
    @DeprecatedName("owner", "use owners instead", "3.0.0")
    owner: String = null,
    owners: Seq[AlignToken.Owner] = Seq.empty
) {
  def getMatcher: Seq[AlignToken.Matcher] = {
    val specs =
      if (owners.isEmpty) Option(owner) match {
        case None => Seq.empty
        case x => Seq(AlignToken.Owner(x))
      }
      else owners.distinct
    specs.map(_.getMatcher)
  }
}

object AlignToken {

  private def pattern(value: String): jurPattern =
    value.r.pattern

  /** @param regex
    *   regexp for class name of scala.meta.Tree "owner".
    * @param parents
    *   optional regexp for class name of owner's parent.
    */
  case class Owner(
      regex: Option[String] = None,
      parents: Seq[String] = Seq.empty
  ) {
    def getMatcher: Matcher =
      new Matcher(regex.map(pattern), parents.map(pattern))
  }
  implicit val ownerSurface = generic.deriveSurface[Owner]
  implicit val ownerCodec = generic.deriveCodecEx(Owner())

  implicit lazy val surface: Surface[AlignToken] =
    generic.deriveSurface[AlignToken]
  implicit lazy val encoder: ConfEncoder[AlignToken] = generic.deriveEncoder
  val applyInfix = "Term.ApplyInfix"
  val caseArrow = AlignToken("=>", "Case")
  protected[scalafmt] val fallbackAlign = new AlignToken("<empty>")
  implicit val decoder: ConfDecoderEx[AlignToken] = {
    val base = generic.deriveDecoderEx[AlignToken](fallbackAlign).noTypos
    ConfDecoderEx.from {
      case (_, Conf.Str("caseArrow")) => Ok(caseArrow)
      case (_, Conf.Str(regex)) =>
        Ok(default.find(_.code == regex).getOrElse(AlignToken(regex)))
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
    AlignToken("=", "(Enumerator.Val|Defn.(Va(l|r)|GivenAlias|Def|Type))")
  )

  class Matcher(val owner: Option[jurPattern], val parents: Seq[jurPattern]) {
    def matches(tree: meta.Tree): Boolean =
      owner.forall(check(tree)) &&
        (parents.isEmpty || tree.parent.exists { p =>
          parents.forall(check(p)) || (p match {
            case ParamClauseParent(pp) => parents.forall(check(pp))
            case _: meta.Member.SyntaxValuesClause =>
              p.parent.exists { pp => parents.forall(check(pp)) }
            case _ => false
          })
        })
  }

  @inline
  private def check(tree: meta.Tree)(pattern: jurPattern): Boolean =
    pattern.matcher(tree.productPrefix).find()

}
