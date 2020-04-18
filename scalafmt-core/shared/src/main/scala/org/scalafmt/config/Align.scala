package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

/**
  *
  * @param openParenCallSite
  *   If true AND bin-packing is true, then call-site
  *   arguments won't be aligned by the opening
  *   parenthesis. For example, this output
  *   will be disallowed
  *
  *   function(a,
  *            b,
  *            c)
  * @param openParenDefnSite Same as [[openParenCallSite]], except definition site.
  * @param tokens The tokens to vertically align by. The "owner" is the
  *               scala.meta.Tree.getClass.getName of the deepest tree node
  *               that "owns" the token to align by.
  *
  *               Examples:
  *
  *               align.tokens = ["="] // align = owned by any tree node (not recommended)
  *
  *               align.tokens = [
  *                 { code = "=", owner = "Param" } // align = when owned by parameter tree nodes
  *               ]
  *
  *               Pro tip. if you use for example
  *
  *               preset = defaultWithAlign
  *
  *               and want to add one extra token (for example "|>") to align by, write
  *
  *               align.tokens.add = [ "|> ]
  *
  *               NOTE. Adding more alignment tokens may potentially decrease the
  *               vertical alignment in formatted output. Customize at your own
  *               risk, I recommend you try and stick to the default settings.
  * @param arrowEnumeratorGenerator If true, aligns by <- in for comprehensions.
  * @param ifWhileOpenParen
  *   If true, aligns by ( in if/while/for. If false,
  *   indents by continuation indent at call site.
  * @param tokenCategory
  *   Customize which token kinds can align together. By default, only tokens with
  *   the same `Token.productPrefix` align. To for example align = and <-,
  *   set the values to:
  *     Map("Equals" -> "Assign", "LeftArrow" -> "Assign")
  *   Note. Requires mixedTokens to be true.
  * @param treeCategory
  *   Customize which tree kinds can align together. By default, only trees with
  *   the same `Tree.productPrefix` align. To for example align Defn.Val and
  *   Defn.Var, set the values to:
  *     Map("Defn.Var" -> "Assign", "Defn.Val" -> "Assign")
  *   Note. Requires mixedOwners to be true.
  * @param stripMargin
  *   If set, indent lines with a strip-margin character in a multiline string
  *   constant relative to the opening quotes (or the strip-margin character if
  *   present) on the first line; otherwise, indent relative to the beginning
  *   of the first line, as usual.
  */
case class Align(
    stripMargin: Boolean = true,
    openParenCallSite: Boolean = false,
    openParenDefnSite: Boolean = false,
    tokens: Seq[AlignToken] = Seq(AlignToken.caseArrow),
    arrowEnumeratorGenerator: Boolean = false,
    ifWhileOpenParen: Boolean = true,
    tokenCategory: Map[String, String] = Map(),
    treeCategory: Map[String, String] = Map(
      "Defn.Val" -> "val/var/def",
      "Defn.Var" -> "val/var/def",
      "Defn.Def" -> "val/var/def",
      "Defn.Class" -> "class/object/trait",
      "Defn.Object" -> "class/object/trait",
      "Defn.Trait" -> "class/object/trait",
      "Enumerator.Generator" -> "for",
      "Enumerator.Val" -> "for"
    )
) extends Decodable[Align] {
  override protected[config] def baseDecoder =
    generic.deriveDecoder(this).noTypos
  implicit def alignReader: ConfDecoder[Seq[AlignToken]] =
    Align.alignTokenReader(tokens)
}

object Align {
  // no vertical alignment whatsoever, if you find any vertical alignment with
  // this settings, please report an issue.
  val none: Align = Align(
    stripMargin = false,
    openParenCallSite = false,
    openParenDefnSite = false,
    tokens = Seq.empty,
    ifWhileOpenParen = false,
    tokenCategory = Map.empty,
    treeCategory = Map.empty
  )
  // stable set of alignment operators, the previous defaultWithAlign.
  val some = Align()
  val default = some
  val more: Align = some.copy(tokens = AlignToken.default)
  implicit lazy val surface: Surface[Align] = generic.deriveSurface[Align]
  implicit lazy val encoder: ConfEncoder[Align] = generic.deriveEncoder

  // only for the truest vertical aligners, this setting is open for changes,
  // please open PR addding more stuff to it if you like.
  val most: Align = more.copy(
    arrowEnumeratorGenerator = true,
    tokenCategory = Map(
      "Equals" -> "Assign",
      "LeftArrow" -> "Assign"
    )
  )
  val allValues = List(default, none, some, most)

  implicit val preset: PartialFunction[Conf, Align] = {
    case Conf.Str("none") | Conf.Bool(false) => Align.none
    case Conf.Str("some" | "default") => Align.some
    case Conf.Str("more") | Conf.Bool(true) => Align.more
    case Conf.Str("most") => Align.most
  }

  def alignTokenReader(
      initTokens: Seq[AlignToken]
  ): ConfDecoder[Seq[AlignToken]] = {
    val base = ConfDecoder[Seq[AlignToken]]
    ConfDecoder.instance[Seq[AlignToken]] {
      case Conf.Obj(("add", c) :: Nil) => base.read(c).map(initTokens ++ _)
      case c => preset.lift(c).fold(base.read(c))(x => Configured.ok(x.tokens))
    }
  }

}
