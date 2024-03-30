package org.scalafmt.config

import metaconfig.ConfCodecEx

sealed abstract class Case {
  import Case._
  def process(str: String): String = this match {
    case Unchanged => str
    case Lower => str.toLowerCase()
    case Upper => str.toUpperCase()
  }
}

object Case {
  implicit val codec: ConfCodecEx[Case] = ReaderUtil
    .oneOf[Case](Upper, Lower, Unchanged)
  case object Upper extends Case
  case object Lower extends Case
  case object Unchanged extends Case
}
