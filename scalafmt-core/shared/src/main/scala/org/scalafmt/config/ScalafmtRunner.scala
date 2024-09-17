package org.scalafmt.config

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.parsers.Parsed

import scala.reflect.ClassTag

import metaconfig._

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
    private[config] val parser: ScalafmtParser = ScalafmtParser.Source,
    optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Int = 1000000,
    private[config] val dialect: NamedDialect = NamedDialect.default,
    private val dialectOverride: Conf.Obj = Conf.Obj.empty,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false,
) {
  @inline
  private[scalafmt] def getDialect = dialect.dialect
  private[scalafmt] lazy val getDialectForParser: Dialect = getDialect
    .withAllowToplevelTerms(true).withAllowToplevelStatements(true)
  @inline
  private[scalafmt] def dialectName = {
    val name = dialect.name
    if (dialectOverride.values.isEmpty) name else s"$name [with overrides]"
  }
  @inline
  private[scalafmt] def getParser = parser.parse

  @inline
  def withDialect(dialect: sourcecode.Text[Dialect]): ScalafmtRunner =
    withDialect(NamedDialect(dialect))

  @inline
  private[scalafmt] def withDialect(dialect: NamedDialect): ScalafmtRunner =
    copy(dialect = dialect)

  @inline
  private[scalafmt] def withParser(parser: ScalafmtParser): ScalafmtRunner =
    copy(parser = parser)

  private[scalafmt] def forCodeBlock: ScalafmtRunner =
    copy(debug = false, eventCallback = null, parser = ScalafmtParser.Source)

  def event(evt: => FormatEvent): Unit =
    if (null != eventCallback) eventCallback(evt)

  def events(evts: => Iterator[FormatEvent]): Unit =
    if (null != eventCallback) evts.foreach(eventCallback)

  def parse(input: meta.inputs.Input): Parsed[_ <: Tree] =
    getParser(input, getDialectForParser)

  @inline
  def isDefaultDialect = dialect.name == NamedDialect.defaultName

  private[scalafmt] def conservative: ScalafmtRunner =
    copy(optimizer = optimizer.conservative)

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
    maxStateVisits = 1000000,
  )

  val sbt = default.withDialect(meta.dialects.Sbt)

  implicit val encoder: ConfEncoder[ScalafmtRunner] = generic.deriveEncoder

  private[config] def overrideDialect[T: ClassTag](
      d: Dialect,
      k: String,
      v: T,
  ) = {
    val methodName =
      if (k.isEmpty || k.startsWith("with")) k
      else "with" + Character.toUpperCase(k.head) + k.tail
    DialectMacro.dialectMap(methodName)(d, v)
  }

  implicit val decoder: ConfDecoderEx[ScalafmtRunner] = generic
    .deriveDecoderEx(default).noTypos.flatMap { runner =>
      val overrides = runner.dialectOverride.values
      if (overrides.isEmpty) Configured.Ok(runner)
      else Configured.fromExceptionThrowing {
        val srcDialect = runner.getDialect
        val dialect = overrides.foldLeft(srcDialect) {
          case (cur, (k, Conf.Bool(v))) => overrideDialect(cur, k, v)
          case (cur, _) => cur // other config types are unsupported
        }
        if (dialect.isEquivalentTo(srcDialect)) runner
        else runner.withDialect(runner.dialect.copy(dialect = dialect))
      }
    }

}
