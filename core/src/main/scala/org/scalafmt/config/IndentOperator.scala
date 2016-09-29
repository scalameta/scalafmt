package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class IndentOperator(
    include: String = ".*",
    exclude: String = "^(&&|\\|\\|)$"
) {
  val includeRegexp = include.r
  val excludeRegexp = exclude.r
}

object IndentOperator {
  val default = IndentOperator()
  val akka = IndentOperator(ScalafmtConfig.indentOperatorsIncludeAkka,
                            ScalafmtConfig.indentOperatorsExcludeAkka)
}
