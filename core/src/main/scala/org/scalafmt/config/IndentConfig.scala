package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class IndentConfig(
    rightAssociativeInfixOperatorsLikeLeftAssociative: Boolean = true
)
