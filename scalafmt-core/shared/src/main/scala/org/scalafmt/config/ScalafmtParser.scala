package org.scalafmt.config

import scala.meta.Tree
import scala.meta.parsers.Parse

sealed class ScalafmtParser(val parse: Parse[_ <: Tree])

object ScalafmtParser {
  case object Case extends ScalafmtParser(Parse.parseCase)
  case object Stat extends ScalafmtParser(Parse.parseStat)
  case object Source extends ScalafmtParser(Parse.parseSource)

  implicit val codec = ReaderUtil.oneOf[ScalafmtParser](Case, Stat, Source)
}
