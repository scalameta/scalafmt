package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

case class Docstrings(
    style: Option[Docstrings.Style] = Some(Docstrings.ScalaDoc)
) {
  import Docstrings._

  def isScalaDoc: Boolean = style.contains(ScalaDoc)

  implicit lazy val decoder: ConfDecoder[Docstrings] = {
    val genericDecoder = generic.deriveDecoder(this).noTypos
    new ConfDecoder[Docstrings] {
      override def read(conf: Conf): Configured[Docstrings] =
        conf match {
          case Conf.Str("preserve") => Configured.ok(copy(style = None))
          case _: Conf.Str => reader.read(conf).map(x => copy(style = Some(x)))
          case _ => genericDecoder.read(conf)
        }
    }
  }

}

object Docstrings {

  implicit val surface: Surface[Docstrings] = generic.deriveSurface[Docstrings]
  implicit val encoder = generic.deriveEncoder[Docstrings]

  sealed abstract class Style
  case object JavaDoc extends Style
  case object ScalaDoc extends Style

  implicit val reader: ConfCodec[Style] =
    ReaderUtil.oneOf[Style](JavaDoc, ScalaDoc)

}
