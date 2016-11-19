package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * @param neverInDanglingParenthesesSingleLineArgList
  *   See https://github.com/olafurpg/scalafmt/issues/593
  *   If false,
  *   function(new Actor {
  *       def foo = 2
  *   })
  *   If true,
  *   function(
  *     new Actor {
  *       def foo = 2
  *     }
  *   )
  */
@ConfigReader
case class Newlines(
    neverInDanglingParenthesesSingleLineArgList: Boolean = false,
    neverInResultType: Boolean = false,
    neverBeforeJsNative: Boolean = false,
    sometimesBeforeColonInMethodReturnType: Boolean = true,
    alwaysBeforeCurlyBraceLambdaParams: Boolean = false
)
