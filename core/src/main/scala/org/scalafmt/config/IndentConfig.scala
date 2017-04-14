package org.scalafmt.config

import metaconfig._

@DeriveConfDecoder
case class IndentConfig(
    rightAssociativeInfixOperatorsLikeLeftAssociative: Boolean = true
)
