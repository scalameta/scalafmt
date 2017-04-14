package org.scalafmt.config

import metaconfig.ConfDecoder

sealed abstract class Docstrings

object Docstrings {
  implicit val reader: ConfDecoder[Docstrings] =
    ReaderUtil.oneOf[Docstrings](JavaDoc, ScalaDoc, preserve)
  case object JavaDoc extends Docstrings
  case object ScalaDoc extends Docstrings
  case object preserve extends Docstrings
}
