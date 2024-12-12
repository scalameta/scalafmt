package org.scalafmt.config

import metaconfig._

/** @param configStyleArguments
  *   Call-sites where there is a newline after opening ( and newline before
  *   closing ). If true, preserves the newlines and keeps one line per
  *   argument.
  *
  * @param breaksInsideChains
  *   NB: failure unless newlines.source=classic If true, then the user can opt
  *   out of line breaks inside select chains.
  *   {{{
  *     // original
  *     foo
  *       .map(_ + 1).map(_ + 1)
  *       .filter(_ > 2)
  *     // if true
  *     foo
  *       .map(_ + 1).map(_ + 1)
  *       .filter(_ > 2)
  *     // if false
  *     foo
  *       .map(_ + 1)
  *       .map(_ + 1)
  *       .filter(_ > 2)
  *   }}}
  *
  * @param breakChainOnFirstMethodDot
  *   NB: ignored unless newlines.source=classic If true, keeps the line break
  *   before a dot if it already exists.
  *   {{{
  *     // original
  *     foo
  *       .map(_ + 1)
  *       .filter( > 2)
  *     // if true
  *     foo
  *       .map(_ + 1)
  *       .filter( > 2)
  *     // if false
  *     foo.map(_ + 1).filter( > 2)
  *   }}}
  *
  * @param encloseClassicChains
  *   NB: ignored unless newlines.source=classic. Controls what happens if a
  *   chain enclosed in parentheses is followed by additional selects. Those
  *   additional selects will be considered part of the enclosed chain if and
  *   only if this flag is false.
  *   {{{
  *     // original
  *     (foo.map(_ + 1).map(_ + 1))
  *       .filter(_ > 2)
  *     // if true
  *     (foo.map(_ + 1).map(_ + 1))
  *       .filter(_ > 2)
  *     // if false
  *     (foo
  *       .map(_ + 1)
  *       .map(_ + 1))
  *       .filter(_ > 2)
  *   }}}
  *
  * @param annotationNewlines
  *   - if newlines.source is missing or keep:
  *     - if true, will keep existing line breaks around annotations
  *   - if newlines.source is fold:
  *     - if true, will break before the entity being annotated
  *     - will not force break between consecutive annotations
  *   - if newlines.source is unfold:
  *     - if true, will break between consecutive annotations
  *     - will always break before the entity being annotated
  *
  * @param selfAnnotationNewline
  *   See https://github.com/scalameta/scalafmt/issues/938 If true, will force a
  *   line break before a self annotation if there was a line break there
  *   before.
  */
case class OptIn(
    @annotation.DeprecatedName(
      "configStyleArguments",
      "Use `newlines.configStyleCallSite` instead",
      "3.8.2",
    )
    private[config] val configStyleArguments: Boolean = true,
    breaksInsideChains: Boolean = false,
    breakChainOnFirstMethodDot: Boolean = true,
    encloseClassicChains: Boolean = false,
    selfAnnotationNewline: Boolean = true,
    annotationNewlines: Boolean = true,
)

object OptIn {
  implicit lazy val surface: generic.Surface[OptIn] = generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[OptIn] = generic.deriveCodecEx(OptIn())
    .noTypos
}
