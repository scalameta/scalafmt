package org.scalafmt.config

import metaconfig.Reader

case class BaseStyle(style: ScalafmtConfig)

object BaseStyle {
  val default: BaseStyle = BaseStyle(ScalafmtConfig.default)
  implicit val styleReader: Reader[BaseStyle] = Reader.instance[BaseStyle] {
    case b: BaseStyle => Right(b) // TODO(olafur) provide this in instance
    case any =>
      println(any)
      for {
        str <- Reader.stringR.read(any).right
        style <- {
          ScalafmtConfig.availableStyles.get(str.toLowerCase) match {
            case Some(s) => Right(s)
            case None =>
              Left(new IllegalArgumentException(
                s"Unknown style name $str. Expected one of ${ScalafmtConfig.activeStyles.keys}"))
          }
        }.right
      } yield BaseStyle(style)
  }
}
