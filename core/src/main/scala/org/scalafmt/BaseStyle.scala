package org.scalafmt

import metaconfig.Reader

case class BaseStyle(style: ScalafmtStyle)

object BaseStyle {
  val default: BaseStyle = BaseStyle(ScalafmtStyle.default)
  implicit val styleReader: Reader[BaseStyle] = Reader.instance[BaseStyle] {
    case b: BaseStyle => Right(b) // TODO(olafur) provide this in instance
    case any =>
      println(any)
      for {
        str <- Reader.stringR.read(any).right
        style <- {
          ScalafmtStyle.availableStyles.get(str.toLowerCase) match {
            case Some(s) => Right(s)
            case None =>
              Left(new IllegalArgumentException(
                s"Unknown style name $str. Expected one of ${ScalafmtStyle.activeStyles.keys}"))
          }
        }.right
      } yield BaseStyle(style)
  }
}
