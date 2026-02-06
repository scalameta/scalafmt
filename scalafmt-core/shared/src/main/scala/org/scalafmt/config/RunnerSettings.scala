package org.scalafmt.config

import scala.meta.parsers.{Parsed, ParserOptions}
import scala.meta.{Dialect, Tree}

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
case class RunnerSettings(
    debug: Boolean = false,
    private val completeCallback: FormatEvent.CompleteFormat => Unit = _ => (),
    private val eventCallback: FormatEvent => Unit = null,
    private[config] val parser: ScalafmtParser = ScalafmtParser.Source,
    optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Option[Int] = None,
    private[config] val dialect: NamedDialect = NamedDialect.default,
    private val dialectOverride: Conf.Obj = Conf.Obj.empty,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false,
    dialectFeatures: Seq[RunnerSettings.DialectFeature] = Nil,
) {
  @inline
  private[scalafmt] def getDialect = dialect.value
  private[scalafmt] implicit lazy val getDialectForParser: Dialect = getDialect
    .withAllowToplevelTerms(true).withAllowToplevelStatements(true)
  @inline
  private[scalafmt] def dialectName = {
    val source = dialect.source
    val name = source.substring(source.lastIndexOf('.') + 1).toLowerCase
    if (dialectOverride.values.isEmpty) name else s"$name [with overrides]"
  }
  @inline
  private[scalafmt] def getParser = parser.parse

  @inline
  def withDialect(nd: NamedDialect): RunnerSettings = copy(dialect = nd)

  @inline
  def withParser(parser: ScalafmtParser): RunnerSettings = copy(parser = parser)

  private[scalafmt] def forCodeBlock =
    copy(debug = false, eventCallback = null, parser = ScalafmtParser.Source)

  private[scalafmt] def withCompleteCallback(
      cb: FormatEvent.CompleteFormat => Unit,
  ) = copy(completeCallback = cb)

  def event(evt: FormatEvent.CompleteFormat): Unit = completeCallback(evt)

  def event(evt: => FormatEvent): Unit =
    if (null != eventCallback) eventCallback(evt)

  private implicit val parserOptions: ParserOptions =
    new ParserOptions(captureComments = false)

  def parse(input: meta.inputs.Input): Parsed[_ <: Tree] = getParser(input)

  @inline
  def isDefaultDialect = dialect.source == NamedDialect.defaultName

  private[scalafmt] def conservative = copy(optimizer = optimizer.conservative)

  private[scalafmt] def getMaxStateVisits: Int = maxStateVisits
    .getOrElse(1000000)

}

object RunnerSettings {
  implicit lazy val surface: generic.Surface[RunnerSettings] =
    generic.deriveSurface

  implicit def formatEventEncoder[A <: FormatEvent]: ConfEncoder[A => Unit] =
    ConfEncoder.StringEncoder.contramap(_ => "<FormatEvent => Unit>")

  /** The default runner formats a compilation unit and listens to no events.
    */
  val default = RunnerSettings()

  val sbt = default.withDialect(meta.dialects.Sbt)

  import NamedDialect.codec

  implicit val encoder: ConfEncoder[RunnerSettings] = generic.deriveEncoder

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

  implicit val decoder: ConfDecoderEx[RunnerSettings] = generic
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
        else runner.withDialect(runner.dialect.copy(value = dialect))
      }
    }

  sealed trait DialectFeature
  object DialectFeature {
    implicit val codec: ConfCodecEx[DialectFeature] = ConfCodecEx
      .oneOf[DialectFeature](relaxedLambdaSyntax)
    case object relaxedLambdaSyntax extends DialectFeature
  }

}
