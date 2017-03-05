package org.scalafmt.config

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse

import metaconfig.ConfigReader
import metaconfig.Reader

/**
  * A FormatRunner configures how formatting should behave.
  *
  * @param debug         Should we collect debugging statistics?
  * @param eventCallback Listen to events that happens while formatting
  * @param parser        Are we formatting a scala.meta.{Source,Stat,Case,...}? For
  *                      more details, see members of [[scala.meta.parsers]].
  */
@ConfigReader
case class ScalafmtRunner(
    debug: Boolean = false,
    eventCallback: FormatEvent => Unit = _ => Unit,
    parser: MetaParser = Parse.parseSource,
    optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Int = 1000000,
    dialect: Dialect = Scala211,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false
) {
  implicit val dialectReader: Reader[Dialect] = ScalafmtRunner.dialectReader
  implicit val optimizerReader: Reader[ScalafmtOptimizer] = optimizer.reader
  implicit val parseReader: Reader[MetaParser] = ScalafmtRunner.parseReader
  implicit val eventReader: Reader[FormatEvent => Unit] =
    ScalafmtRunner.eventReader

}

object ScalafmtRunner extends ScalafmtRunnerT
