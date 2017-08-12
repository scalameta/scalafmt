package org.scalafmt.config

import metaconfig._

@DeriveConfDecoder
case class IncludeFixesFrom(
    v1_2_0: Boolean = true
)

object IncludeFixesFrom {
  val default = IncludeFixesFrom()
}
