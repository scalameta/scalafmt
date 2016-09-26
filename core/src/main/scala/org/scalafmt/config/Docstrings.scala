package org.scalafmt.config

sealed abstract class Docstrings

object Docstrings {
  val reader = ReaderUtil.oneOf[Docstrings](JavaDoc, ScalaDoc, preserve)
  case object JavaDoc extends Docstrings
  case object ScalaDoc extends Docstrings
  case object preserve extends Docstrings
}
