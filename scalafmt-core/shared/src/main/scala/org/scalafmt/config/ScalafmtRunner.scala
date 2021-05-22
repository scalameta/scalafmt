package org.scalafmt.config

import metaconfig._
import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.parsers.Parsed

/** A FormatRunner configures how formatting should behave.
  *
  * @param debug         Should we collect debugging statistics?
  * @param eventCallback Listen to events that happens while formatting
  * @param parser        Are we formatting a scala.meta.{Source,Stat,Case,...}? For
  *                      more details, see members of [[scala.meta.parsers]].
  */
case class ScalafmtRunner(
    debug: Boolean = false,
    private val eventCallback: FormatEvent => Unit = null,
    parser: ScalafmtParser = ScalafmtParser.Source,
    optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Int = 1000000,
    dialect: Dialect = ScalafmtRunner.Dialect.default,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false
) {
  def forSbt: ScalafmtRunner =
    copy(
      dialect = dialect
        .withAllowToplevelTerms(true)
        .withToplevelSeparator("")
    )

  def event(evt: => FormatEvent): Unit =
    if (null != eventCallback) eventCallback(evt)

  def events(evts: => Iterator[FormatEvent]): Unit =
    if (null != eventCallback) evts.foreach(eventCallback)

  def parse(input: meta.inputs.Input): Parsed[_ <: Tree] =
    dialect(input).parse(parser.parse)

}

object ScalafmtRunner {
  implicit lazy val surface: generic.Surface[ScalafmtRunner] =
    generic.deriveSurface
  implicit lazy val formatEventEncoder: ConfEncoder[FormatEvent => Unit] =
    ConfEncoder.StringEncoder.contramap(_ => "<FormatEvent => Unit>")

  /** The default runner formats a compilation unit and listens to no events.
    */
  val default = ScalafmtRunner(
    debug = false,
    parser = ScalafmtParser.Source,
    optimizer = ScalafmtOptimizer.default,
    maxStateVisits = 1000000
  )

  val sbt = default.forSbt

  object Dialect {
    import scala.meta.dialects._

    val scala212 = Scala212
      .withAllowTrailingCommas(true) // SIP-27, 2.12.2
    val scala213 = Scala213
      .withAllowTrailingCommas(true)
    val default = scala213
    val scala3 = Scala3

    val codec = ReaderUtil.oneOf[Dialect](
      default,
      Scala211,
      scala212,
      scala213,
      Scala213Source3,
      Sbt0137,
      Sbt1,
      Dotty,
      scala3,
      Paradise211,
      Paradise212
    )
  }

  implicit val dialectCodec = Dialect.codec
  implicit val codec: ConfCodecEx[ScalafmtRunner] =
    generic.deriveCodecEx(default).noTypos

}
