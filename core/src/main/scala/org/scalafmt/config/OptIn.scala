package org.scalafmt.config

import metaconfig.ConfigReader

/**
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
@ConfigReader
case class OptIn(
    configStyleArguments: Boolean = true,
    breakChainOnFirstMethodDot: Boolean = true,
    annotationNewlines: Boolean = false
)
