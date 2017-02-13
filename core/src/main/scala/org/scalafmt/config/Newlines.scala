package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * @param penalizeSingleSelectMultiArgList
  *   If true, adds a penalty to newlines before a dot starting a select
  *   chain of length one and argument list. The penalty matches the number
  *   of arguments to the select chain application.
  *   {{{
  *     // If true, favor
  *     logger.elem(a,
  *                 b,
  *                 c)
  *     // instead of
  *     logger
  *       .elem(a, b, c)
  *
  *     // penalty is proportional to argument count, example:
  *     logger.elem(a, b, c)    // penalty 2
  *     logger.elem(a, b, c, d) // penalty 3, etc.
  *   }}}
  *
  *   If false, matches pre-v0.5 behavior. Note. this option may be
  *   removed in a future release.
  */
@ConfigReader
case class Newlines(
    neverInResultType: Boolean = false,
    neverBeforeJsNative: Boolean = false,
    sometimesBeforeColonInMethodReturnType: Boolean = true,
    penalizeSingleSelectMultiArgList: Boolean = true,
    alwaysBeforeCurlyBraceLambdaParams: Boolean = false,
    alwaysBeforeTopLevelStatements: Boolean = false
)
