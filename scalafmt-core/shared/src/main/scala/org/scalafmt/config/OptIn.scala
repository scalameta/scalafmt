package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

/**
  *
  * @param configStyleArguments Call-sites where there is a newline after
  *                             opening ( and newline before closing ).
  *                             If true, preserves the newlines and keeps one
  *                             line per argument.
  * @param breaksInsideChains
  *  NB: failure unless newlines.source=classic
  *  If true, then the user can opt out of line breaks
  *  inside select chains.
  *
  *  {{{
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
  * }}}
  *
  * @param breakChainOnFirstMethodDot
  *   NB: ignored unless newlines.source=classic
  *   If true, keeps the line break before a dot if it already exists.
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
  * @param annotationNewlines
  *   - if newlines.source is missing or keep:
  *     - if true, will keep existing line breaks around annotations
  *   - if newlines.source is fold:
  *     - if true, will break before the entity being annotatated
  *     - will not force break between consecutive annotations
  *   - if newlines.source is unfold:
  *     - if true, will break between consecutive annotations
  *     - will always break before the entity being annotatated
  *
  * @param selfAnnotationNewline See https://github.com/scalameta/scalafmt/issues/938
  *                              If true, will force a line break before a self annotation
  *                              if there was a line break there before.
  * @param forceBlankLineBeforeDocstring
  *  If true, always insert a blank line before docstrings,
  *  If false, preserves blank line only if one exists before.
  *  Example:
  *  {{{
  *    // before
  *    object Foo {
  *      /** Docstring */
  *      def foo = 2
  *    }
  *    // after, if forceBlankLineBeforeDocstring=false
  *    object Foo {
  *      /** Docstring */
  *      def foo = 2
  *    }
  *    // after, if forceBlankLineBeforeDocstring=true
  *    object Foo {
  *
  *      /** Docstring */
  *      def foo = 2
  *    }
  *  }}}
  */
case class OptIn(
    configStyleArguments: Boolean = true,
    breaksInsideChains: Boolean = false,
    breakChainOnFirstMethodDot: Boolean = true,
    selfAnnotationNewline: Boolean = true,
    annotationNewlines: Boolean = true,
    // Candidate to become default false at some point.
    forceBlankLineBeforeDocstring: Boolean = true,
    @annotation.DeprecatedName(
      "blankLineBeforeDocstring",
      "Use optIn.forceBlankLineBeforeDocstring instead",
      "2.5.0"
    )
    blankLineBeforeDocstring: Boolean = false
) {
  implicit val reader: ConfDecoder[OptIn] = generic.deriveDecoder(this).noTypos

  /**
    * See https://github.com/scalameta/scalafmt/issues/1712
    *
    * Setting behavior and name were mirrored. After deprecation and right naming
    * we need to:
    * if `forceBlankLineBeforeDocstring` (new name) has default value (true)
    *   fallback to `blankLineBeforeDocstring` (old config) which may be
    *   configured in .scalafmt.conf
    * if `forceBlankLineBeforeDocstring` configured to non-default value
    *   don't look at the old name
    * */
  lazy val forceNewlineBeforeDocstringSummary: Boolean =
    forceBlankLineBeforeDocstring && !blankLineBeforeDocstring
}

object OptIn {
  implicit lazy val surface: Surface[OptIn] = generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[OptIn] = generic.deriveEncoder
}
