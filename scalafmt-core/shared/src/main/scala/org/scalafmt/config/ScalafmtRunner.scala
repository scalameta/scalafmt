package org.scalafmt.config

import metaconfig._
import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.parsers.Parsed

/** A FormatRunner configures how formatting should behave.
  *
  * @param debug
  *   Should we collect debugging statistics?
  * @param eventCallback
  *   Listen to events that happens while formatting
  * @param parser
  *   Are we formatting a scala.meta.{Source,Stat,Case,...}? For more details,
  *   see members of [[scala.meta.parsers]].
  */
case class ScalafmtRunner(
    debug: Boolean = false,
    private val eventCallback: FormatEvent => Unit = null,
    parser: ScalafmtParser = ScalafmtParser.Source,
    optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Int = 1000000,
    dialect: ScalafmtRunner.Dialect.WithName = ScalafmtRunner.Dialect.default,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false
) {
  @inline def getDialect = dialect.value

  def topLevelDialect = dialect.copy(
    value = getDialect
      .withAllowToplevelTerms(true)
      .withToplevelSeparator("")
  )

  def forSbt: ScalafmtRunner = copy(dialect = topLevelDialect)

  def event(evt: => FormatEvent): Unit =
    if (null != eventCallback) eventCallback(evt)

  def events(evts: => Iterator[FormatEvent]): Unit =
    if (null != eventCallback) evts.foreach(eventCallback)

  def parse(input: meta.inputs.Input): Parsed[_ <: Tree] =
    parser.parse(input, getDialect)

  def isDefaultDialect = dialect.source == ScalafmtRunner.Dialect.defaultName
  def warnDefault: Unit = if (isDefaultDialect) {
    val known = ScalafmtRunner.Dialect.known.map(_.source).mkString(",")
    Console.err.print(
      s"""Default dialect is deprecated; use explicit: [$known]
        |Also see https://scalameta.org/scalafmt/docs/configuration.html#scala-dialects"
        |""".stripMargin
    )
  }

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
    type WithName = sourcecode.Text[Dialect]
    def withName(name: String, dialect: Dialect): WithName =
      sourcecode.Text(dialect, name)

    import scala.meta.dialects._

    val scala212 = Scala212
      .withAllowTrailingCommas(true) // SIP-27, 2.12.2
    val scala213 = Scala213
      .withAllowTrailingCommas(true)
    val scala3 = Scala3

    private[ScalafmtRunner] val known: Seq[WithName] = Seq(
      Scala211,
      scala212,
      Scala212Source3,
      scala213,
      Scala213Source3,
      Sbt0137,
      Sbt1,
      scala3
    )

    private[ScalafmtRunner] val defaultName = "default"
    // current default is 213
    private[ScalafmtRunner] val default = withName(defaultName, scala213)
  }

  implicit val dialectCodec = {
    val dialects = (Dialect.known :+ Dialect.default)
      .map(x => sourcecode.Text(x, x.source))
    ReaderUtil.oneOf(dialects: _*)
  }

  implicit val codec: ConfCodecEx[ScalafmtRunner] =
    generic.deriveCodecEx(default).noTypos

}
