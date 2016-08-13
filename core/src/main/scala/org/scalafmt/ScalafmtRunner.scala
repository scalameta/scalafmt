package org.scalafmt

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.parsers.Parse

import org.scalafmt.FormatEvent.CompleteFormat
import org.scalafmt.FormatEvent.Enqueue
import org.scalafmt.FormatEvent.Explored
import org.scalafmt.FormatEvent.VisitToken
import org.scalafmt.util.LoggerOps

/**
  * A FormatRunner configures how formatting should behave.
  *
  * @param debug         Should we collect debugging statistics?
  * @param eventCallback Listen to events that happens while formatting
  * @param parser        Are we formatting a scala.meta.{Source,Stat,Case,...}? For
  *                      more details, see members of [[scala.meta.parsers]].
  */
case class ScalafmtRunner(debug: Boolean,
                          eventCallback: FormatEvent => Unit,
                          parser: Parse[_ <: Tree],
                          optimizer: ScalafmtOptimizer,
                          maxStateVisits: Int,
                          dialect: Dialect) {

  def withParser(newParser: Parse[_ <: Tree]): ScalafmtRunner =
    this.copy(parser = newParser)
}

object ScalafmtRunner {
  import LoggerOps._

  /**
    * The default runner formats a compilation unit and listens to no events.
    */
  val default = ScalafmtRunner(debug = false,
                               eventCallback = _ => Unit,
                               parser = scala.meta.parsers.Parse.parseSource,
                               optimizer = ScalafmtOptimizer.default,
                               maxStateVisits = 1000000,
                               scala.meta.dialects.Scala211)

  /**
    * Same as [[default]], except formats the input as a statement/expression.
    *
    * An example of how to format something other than a compilation unit.
    */
  val statement = default.withParser(scala.meta.parsers.Parse.parseStat)

  val sbt = default.copy(dialect = scala.meta.dialects.Sbt0137)
}
