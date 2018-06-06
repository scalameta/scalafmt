package org.scalafmt.config

import metaconfig._
import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.dialects.Scala211
import scala.meta.parsers.Parse

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
    eventCallback: FormatEvent => Unit = _ => Unit,
    parser: Parse[_ <: Tree] = Parse.parseSource,
    optimizer: ScalafmtOptimizer = ScalafmtOptimizer.default,
    maxStateVisits: Int = 1000000,
    dialect: Dialect = ScalafmtRunner.defaultDialect,
    ignoreWarnings: Boolean = false,
    fatalWarnings: Boolean = false
) {
  implicit val optimizeDecoder = optimizer.reader
  val reader: ConfDecoder[ScalafmtRunner] = generic.deriveDecoder(this).noTypos
  def forSbt: ScalafmtRunner =
    copy(
      dialect = dialect.copy(
        allowToplevelTerms = true,
        toplevelSeparator = ""
      )
    )
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
  val defaultDialect = Scala211.copy(
    // Are `&` intersection types supported by this dialect?
    allowAndTypes = true,
    // Are extractor varargs specified using ats, i.e. is `case Extractor(xs @ _*)` legal or not?
    allowAtForExtractorVarargs = true,
    // Are extractor varargs specified using colons, i.e. is `case Extractor(xs: _*)` legal or not?
    allowColonForExtractorVarargs = true,
    // Are `inline` identifiers supported by this dialect?
    allowInlineIdents = true,
    // Are inline vals and defs supported by this dialect?
    allowInlineMods = false,
    // Are literal types allowed, i.e. is `val a : 42 = 42` legal or not?
    allowLiteralTypes = true,
    // Are `|` (union types) supported by this dialect?
    allowOrTypes = true,
    // Are trailing commas allowed? SIP-27.
    allowTrailingCommas = true,
    // Are trait allowed to have parameters?
    // They are in Dotty, but not in Scala 2.12 or older.
    allowTraitParameters = true,
    // Are view bounds supported by this dialect?
    // Removed in Dotty.
    allowViewBounds = true,
    // Are `with` intersection types supported by this dialect?
    allowWithTypes = true,
    // Are XML literals supported by this dialect?
    // We plan to deprecate XML literal syntax, and some dialects
    // might go ahead and drop support completely.
    allowXmlLiterals = true
  )

  /**
    * The default runner formats a compilation unit and listens to no events.
    */
  val default = ScalafmtRunner(
    debug = false,
    eventCallback = _ => Unit,
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

}
