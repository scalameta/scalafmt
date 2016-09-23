package org.scalafmt

import metaconfig.ConfigReader

@ConfigReader
case class IndentOperator(include: String, exclude: String) {
  val includeRegexp = include.r
  val excludeRegexp = exclude.r
}

object IndentOperator {
  val default = IndentOperator(ScalafmtStyle.indentOperatorsIncludeDefault,
                               ScalafmtStyle.indentOperatorsExcludeDefault)
  val akka = IndentOperator(ScalafmtStyle.indentOperatorsIncludeAkka,
                            ScalafmtStyle.indentOperatorsExcludeAkka)
}
