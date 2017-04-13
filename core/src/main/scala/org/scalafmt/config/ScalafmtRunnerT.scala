package org.scalafmt.config

import scala.meta.Dialect
import scala.meta.dialects.Dotty
import scala.meta.dialects.Paradise211
import scala.meta.dialects.Sbt0137
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse

import metaconfig._

trait ScalafmtRunnerT {

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

  lazy val eventReader: ConfDecoder[FormatEvent => Unit] =
    ConfDecoder.instance[FormatEvent => Unit] {
      case _ =>
        Configured.Ok((_: FormatEvent) => Unit)
    }
  lazy val parseReader: ConfDecoder[MetaParser] = {
    import Parse._
    ReaderUtil.oneOf[MetaParser](parseSource, parseStat, parseCase)
  }

  lazy val dialectReader: ConfDecoder[Dialect] =
    ReaderUtil.oneOf[Dialect](Scala211, Sbt0137, Dotty, Paradise211)
}
