package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * @param breakChainOnFirstMethodDot
  *   If true, keeps the line break before a dot if it already exists.
  *   {{{
  *     // original
  *     foo
  *       .map(_ + 1)
  *       .filter(_ > 2)
  *     // if true
  *     foo
  *       .map(_ + 1)
  *       .filter(_ > 2)
  *     // if false
  *     foo.map(_ + 1).filter(_ > 2)
  *   }}}
  *  @param noNewlineAfterAnnotation
  *   If true, don't move annotations to new line.
  *   {{{
  *     // original
  *      @annot @deprecated class B extends A
  *     // if true
  *      @annot @deprecated class B extends A
  *     // if false
  *     @annot
  *     @deprecated
  *     class B extends A
  *   }}}
  */
@ConfigReader
case class OptIn(
    configStyleArguments: Boolean = true,
    breakChainOnFirstMethodDot: Boolean = true,
    noNewlineAfterAnnotation: Boolean = false
)
