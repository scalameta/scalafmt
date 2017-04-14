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
  */
@DeriveConfDecoder
case class Align(
    openParenCallSite: Boolean = true,
    openParenDefnSite: Boolean = true,
    mixedOwners: Boolean = false,
    tokens: Set[AlignToken] = Set.empty[AlignToken],
    arrowEnumeratorGenerator: Boolean = false,
    ifWhileOpenParen: Boolean = true
) {
  implicit val alignReader: ConfDecoder[Set[AlignToken]] =
    ScalafmtConfig.alignReader(tokens)
}
