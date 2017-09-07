package org.scalafmt.config

import metaconfig._

/**
  *
  * @param configStyleArguments Call-sites where there is a newline after
  *                             opening ( and newline before closing ).
  *                             If true, preserves the newlines and keeps one
  *                             line per argument.
  * @param breaksInsideChains
  *  If true, then the user can opt out of line breaks
  *  inside select chains.
  *
  *  {{{
  *     // original
  *     foo
  *       .map(_ + 1).map(_ + 1)
  *       .filter( > 2)
  *     // if true
  *     foo
  *       .map(_ + 1).map(_ + 1)
  *       .filter( > 2)
  *     // if false
  *     foo
  *       .map(_ + 1)
  *       .map(_ + 1)
  *       .filter( > 2)
  * }}}
  *
  * @param breakChainOnFirstMethodDot
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
  * @param selfAnnotationNewline See https://github.com/scalameta/scalafmt/issues/938
  *                              If true, will force a line break before a self annotation
  *                              if there was a line break there before.
  */
@DeriveConfDecoder
case class OptIn(
    configStyleArguments: Boolean = true,
    breaksInsideChains: Boolean = false,
    breakChainOnFirstMethodDot: Boolean = true,
    selfAnnotationNewline: Boolean = true,
    annotationNewlines: Boolean = true
)
