package org.scalafmt.config

import metaconfig._

/**
  *
  * @param configStyleArguments Call-sites where there is a newline after
  *                             opening ( and newline before closing ).
  *                             If true, preserves the newlines and keeps one
  *                             line per argument.
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
  */
@DeriveConfDecoder
case class OptIn(
    configStyleArguments: Boolean = true,
    breakChainOnFirstMethodDot: Boolean = true,
    annotationNewlines: Boolean = true
)
