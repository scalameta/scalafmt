package org.scalafmt.config

import metaconfig._

case class XmlLiterals(assumeFormatted: Boolean = false)

object XmlLiterals {
  implicit val surface: generic.Surface[XmlLiterals] = generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[XmlLiterals] = generic
    .deriveCodecEx(XmlLiterals()).noTypos
}
