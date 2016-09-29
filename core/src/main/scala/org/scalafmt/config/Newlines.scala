package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class Newlines(
    neverBeforeJsNative: Boolean = false,
    sometimesBeforeColonInMethodReturnType: Boolean = true,
    alwaysBeforeCurlyBraceLambdaParams: Boolean = false
)
