package org.scalafmt.config

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse

import metaconfig._

import scala.meta.dialects.Dotty
import scala.meta.dialects.Paradise211
import scala.meta.dialects.Sbt0137
import scala.meta.dialects.Scala211

/**
  * A FormatRunner configures how formatting should behave.
  *
  * @param debug         Should we collect debugging statistics?
  * @param eventCallback Listen to events that happens while formatting
  * @param parser        Are we formatting a scala.meta.{Source,Stat,Case,...}? For
  *                      more details, see members of [[scala.meta.parsers]].
  */
@DeriveConfDecoder
case class ScalafmtRunner(
    debug: Boolean = false,
    eventCallback: FormatEvent => Unit = _ => Unit,
    parser: MetaParser = Parse.parseSource,
    @Recurse optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Int = 1000000,
    dialect: Dialect = Scala211,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false
)

object ScalafmtRunner {

  /**
    * The default runner formats a compilation unit and listens to no events.
    */
  val default = ScalafmtRunner(
    debug = false,
    eventCallback = _ => Unit,
    parser = scala.meta.parsers.Parse.parseSource,
    optimizer = ScalafmtOptimizer.default,
    maxStateVisits = 1000000,
    scala.meta.dialects.Scala211
  )

  /**
    * Same as [[default]], except formats the input as a statement/expression.
    *
    * An example of how to format something other than a compilation unit.
    */
  val statement = default.copy(parser = scala.meta.parsers.Parse.parseStat)

  val sbt = default.copy(dialect = scala.meta.dialects.Sbt0137)

}
