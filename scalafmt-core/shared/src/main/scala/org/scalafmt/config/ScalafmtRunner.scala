package org.scalafmt.config

import metaconfig._
import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.parsers.Parse
import scala.meta.parsers.Parsed

/**
  * A FormatRunner configures how formatting should behave.
  *
  * @param debug         Should we collect debugging statistics?
  * @param eventCallback Listen to events that happens while formatting
  * @param parser        Are we formatting a scala.meta.{Source,Stat,Case,...}? For
  *                      more details, see members of [[scala.meta.parsers]].
  */
case class ScalafmtRunner(
    debug: Boolean = false,
    private val eventCallback: FormatEvent => Unit = null,
    parser: Parse[_ <: Tree] = Parse.parseSource,
    optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Int = 1000000,
    dialect: Dialect = ScalafmtRunner.Dialect.default,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false
) {
  implicit val optimizeDecoder = optimizer.reader
  implicit def dialectDecoder = ScalafmtRunner.Dialect.decoder
  val reader: ConfDecoder[ScalafmtRunner] = generic.deriveDecoder(this).noTypos
  def forSbt: ScalafmtRunner =
    copy(
      dialect = dialect
        .withAllowToplevelTerms(true)
        .withToplevelSeparator("")
    )

  private lazy val correctedDialect: Dialect = {
    // without allowTraitParameters, our code handling "extends" wouldn't work
    // owner of "extends" would be Name.Anonymous, expects Trait or Template
    dialect.withAllowTraitParameters(true)
  }

  def event(evt: => FormatEvent): Unit =
    if (null != eventCallback) eventCallback(evt)

  def events(evts: => Iterator[FormatEvent]): Unit =
    if (null != eventCallback) evts.foreach(eventCallback)

  def parse(input: meta.inputs.Input): Parsed[_ <: Tree] =
    correctedDialect(input).parse(parser)

}

object ScalafmtRunner {
  implicit lazy val surface: generic.Surface[ScalafmtRunner] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[ScalafmtRunner] =
    generic.deriveEncoder
  implicit lazy val formatEventEncoder: ConfEncoder[FormatEvent => Unit] =
    ConfEncoder.StringEncoder.contramap(_ => "<FormatEvent => Unit>")
  implicit lazy val parseEncoder: ConfEncoder[Parse[_ <: Tree]] =
    ConfEncoder.StringEncoder.contramap(_ => "<Parse[Tree]>")
  implicit lazy val dialectEncoder: ConfEncoder[Dialect] =
    ConfEncoder.StringEncoder.contramap(_ => "<Dialect>")

  /**
    * The default runner formats a compilation unit and listens to no events.
    */
  val default = ScalafmtRunner(
    debug = false,
    parser = scala.meta.parsers.Parse.parseSource,
    optimizer = ScalafmtOptimizer.default,
    maxStateVisits = 1000000
  )

  /**
    * Same as [[default]], except formats the input as a statement/expression.
    *
    * An example of how to format something other than a compilation unit.
    */
  val statement = default.copy(parser = scala.meta.parsers.Parse.parseStat)

  val sbt = default.forSbt

  object Dialect {
    import scala.meta.dialects._

    val scala212 = Scala212
      .withAllowTrailingCommas(true) // SIP-27, 2.12.2
    val scala213 = Scala213
      .withAllowTrailingCommas(true)
    val default = scala213
      .withAllowOrTypes(true) // New feature in Dotty
      .withAllowAndTypes(true) // New feature in Dotty
      .withAllowTraitParameters(true) // New feature in Dotty
      .withAllowColonForExtractorVarargs(true) // New feature in Dotty

    implicit lazy val decoder: ConfDecoder[Dialect] = {
      ReaderUtil.oneOf[Dialect](
        default,
        Scala211,
        scala212,
        scala213,
        Sbt0137,
        Sbt1,
        Dotty,
        Paradise211,
        Paradise212
      )
    }

  }

}
