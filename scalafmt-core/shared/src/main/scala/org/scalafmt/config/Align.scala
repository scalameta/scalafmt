package org.scalafmt.config

import scala.meta._

import metaconfig._

/** @param openParenCallSite
  *   If true AND bin-packing is true, then call-site arguments won't be aligned
  *   by the opening parenthesis. For example, this output will be disallowed
  *   {{{
  *   function1(a,
  *             b,
  *             c)
  *   }}}
  *
  * @param openParenDefnSite
  *   Same as [[openParenCallSite]], except definition site.
  *
  * @param tokens
  *   The tokens to vertically align by. The "owner" is the
  *   scala.meta.Tree.getClass.getName of the deepest tree node that "owns" the
  *   token to align by.
  *   - Examples:
  *     {{{
  *   align.tokens = ["="] // align = owned by any tree node (not recommended)
  *
  *   align.tokens = [
  *     { code = "=", owner = "Param" } // align = when owned by parameter tree nodes
  *   ]
  *     }}}
  *   - Pro tip. if you use for example {{{preset = defaultWithAlign}}} and want
  *     to add one extra token (for example "|>") to align by, write
  *     {{{
  *     align.tokens."+" = [ "|> ]
  *     }}}
  *   - NOTE. Adding more alignment tokens may potentially decrease the vertical
  *     alignment in formatted output. Customize at your own risk, I recommend
  *     you try and stick to the default settings.
  *
  * @param arrowEnumeratorGenerator
  *   If true, aligns by `<-` in for comprehensions.
  *
  * @param openParenCtrlSite
  *   If true, aligns by ( in if/while/for. If false, indents by continuation
  *   indent at call site.
  *
  * @param tokenCategory
  *   Customize which token kinds can align together. By default, only tokens
  *   with the same `Token.productPrefix` align. For example, to align = and <-,
  *   set the values to:
  *   {{{
  *     Map("Equals" -> "Assign", "LeftArrow" -> "Assign")
  *   }}}
  *   Note: Requires mixedTokens to be true.
  *
  * @param treeCategory
  *   Customize which tree kinds can align together. By default, only trees with
  *   the same `Tree.productPrefix` align. For example, to align Defn.Val and
  *   Defn.Var, set the values to:
  *   {{{
  *     Map("Defn.Var" -> "Assign", "Defn.Val" -> "Assign")
  *   }}}
  *   Note. Requires mixedOwners to be true.
  *
  * @param stripMargin
  *   If set, indent lines with a strip-margin character in a multiline string
  *   constant relative to the opening quotes (or the strip-margin character if
  *   present) on the first line; otherwise, indent relative to the beginning of
  *   the first line, as usual.
  */
case class Align(
    allowOverflow: Boolean = false,
    delayUntilSpace: Boolean = true,
    multiline: Boolean = false,
    stripMargin: Boolean = true,
    closeParenSite: Boolean = false,
    private val openBracketCallSite: Option[Boolean] = None,
    openParenCallSite: Boolean = false,
    @annotation.ExtraName("ifWhileOpenParen")
    openParenCtrlSite: Boolean = false,
    private val openBracketDefnSite: Option[Boolean] = None,
    openParenDefnSite: Boolean = false,
    private[config] val openParenTupleSite: Option[Boolean] = None,
    beforeOpenParenDefnSite: Boolean = false,
    beforeOpenParenCallSite: Boolean = false,
    inInterpolation: Boolean = false,
    tokens: Seq[AlignToken] = Seq(AlignToken.caseArrow),
    arrowEnumeratorGenerator: Boolean = false,
    tokenCategory: Map[String, String] = Map(),
    treeCategory: Map[String, String] = Map(
      "Defn.Val" -> "given/val/var/def",
      "Defn.Var" -> "given/val/var/def",
      "Decl.Def" -> "given/val/var/def",
      "Defn.Def" -> "given/val/var/def",
      "Defn.Macro" -> "given/val/var/def",
      "Defn.GivenAlias" -> "given/val/var/def",
      "Defn.Class" -> "class/object/trait/enum",
      "Defn.Object" -> "class/object/trait/enum",
      "Defn.Trait" -> "class/object/trait/enum",
      "Defn.Enum" -> "class/object/trait/enum",
      "Enumerator.Generator" -> "for",
      "Enumerator.Val" -> "for",
    ),
) {

  def atDefnSite(owner: Tree): Boolean = (owner match {
    case _: Type.ParamClause => openBracketDefnSite
    case _ => None
  }).getOrElse(openParenDefnSite)

  def atCallSite(owner: Tree): Boolean = (owner match {
    case _: Member.Tuple => openParenTupleSite
    case _: Type.ArgClause => openBracketCallSite
    case _ => None
  }).getOrElse(openParenCallSite)

}

object Align {
  // no vertical alignment whatsoever, if you find any vertical alignment with
  // this settings, please report an issue.
  val none: Align = Align(
    stripMargin = false,
    openParenCallSite = false,
    openParenCtrlSite = false,
    openParenDefnSite = false,
    tokens = Seq.empty,
    tokenCategory = Map.empty,
    treeCategory = Map.empty,
  )
  // stable set of alignment operators, the previous defaultWithAlign.
  val some = Align()
  val default = some
  val more: Align = some.copy(tokens = AlignToken.default)
  implicit lazy val surface: generic.Surface[Align] = generic
    .deriveSurface[Align]
  implicit lazy val encoder: ConfEncoder[Align] = generic.deriveEncoder
  implicit lazy val decoder: ConfDecoderEx[Align] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "align")

  // only for the truest vertical aligners, this setting is open for changes,
  // please open PR adding more stuff to it if you like.
  val most: Align = more.copy(
    allowOverflow = true,
    multiline = true,
    arrowEnumeratorGenerator = true,
    tokenCategory = Map("Equals" -> "Assign", "LeftArrow" -> "Assign"),
  )
  val allValues = List(default, none, some, most)

  implicit val preset: PartialFunction[Conf, Align] = {
    case Conf.Str("none") | Conf.Bool(false) => Align.none
    case Conf.Str("some" | "default") => Align.some
    case Conf.Str("more") | Conf.Bool(true) => Align.more
    case Conf.Str("most") => Align.most
  }

  implicit val alignTokensDecoder: ConfDecoderEx[Seq[AlignToken]] = {
    val base = AlignToken.seqDecoder
    ConfDecoderEx.from {
      // this is really no longer necessary; metaconfig supports "+" key
      case (state, Conf.Obj(List(("add", c)))) =>
        Console.err.println(
          """'align.tokens.add' is deprecated; use align.tokens."+" instead.""",
        )
        base.read(None, c).map(x => state.fold(x)(_ ++ x))
      case (state, c) => preset.lift(c)
          .fold(base.read(state, c))(x => Configured.Ok(x.tokens))
    }
  }

}
