package org.scalafmt.config

import metaconfig._, Configured._

case class BaseStyle(style: ScalafmtConfig)

object BaseStyle {
  val default: BaseStyle = BaseStyle(ScalafmtConfig.default)
  implicit val styleReader: ConfDecoder[BaseStyle] =
    ConfDecoder.instance[BaseStyle] {
      case any =>
        for {
          str <- ConfDecoder.stringConfDecoder.read(any)
          style <- {
            ScalafmtConfig.availableStyles.get(str.toLowerCase) match {
              case Some(s) => Ok(s)
              case None =>
                ConfError
                  .msg(
                    s"Unknown style name $str. Expected one of ${ScalafmtConfig.activeStyles.keys}")
                  .notOk
            }
          }
        } yield BaseStyle(style)
    }
}
