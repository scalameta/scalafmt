package org.scalafmt.config

import metaconfig.ConfCodecEx

import scala.meta._
import scala.meta.parsers.Parse
import scala.meta.parsers.Parsed

sealed class ScalafmtParser(val parse: Parse[_ <: Tree])

object ScalafmtParser {
  case object Case extends ScalafmtParser(Parse.parseCase)
  case object Stat extends ScalafmtParser(Parse.parseStat)
  case object Source extends ScalafmtParser(SourceParser)

  implicit val codec: ConfCodecEx[ScalafmtParser] =
    ReaderUtil.oneOf[ScalafmtParser](Case, Stat, Source)

  private object SourceParser extends Parse[Tree] {
    override def apply(input: Input, dialect: Dialect): Parsed[Tree] = {
      val isAmmonite = input.isInstanceOf[Input.Ammonite]
      val parser = if (isAmmonite) Parse.parseAmmonite else Parse.parseSource
      parser(input, dialect)
    }
  }
}
