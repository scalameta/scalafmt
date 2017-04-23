package org.scalafmt.config

import metaconfig._

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
  * @param mixedOwners
  *  If true, aligns `=` for val/var/def and
  *  `extends` for def/trait/object.
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
  *               style = defaultWithAlign
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
  */
@DeriveConfDecoder
case class Align(
    openParenCallSite: Boolean = true,
    openParenDefnSite: Boolean = true,
    // TODO(olafur) deprecate these in favor of tokenCategory/treeCategory
    mixedOwners: Boolean = false,
    mixedTokens: Boolean = false,
    tokens: Set[AlignToken] = Set.empty[AlignToken],
    arrowEnumeratorGenerator: Boolean = false,
    ifWhileOpenParen: Boolean = true,
    tokenCategory: Map[String, String] = Map(
      "Equals" -> "Assign",
      "LeftArrow" -> "Assign"
    ),
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
) {
  implicit val alignReader: ConfDecoder[Set[AlignToken]] =
    ScalafmtConfig.alignReader(tokens)
}
