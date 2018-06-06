package org.scalafmt.config

import metaconfig._, Configured._

case class BaseStyle(style: ScalafmtConfig)

object BaseStyle {
  val default: BaseStyle = BaseStyle(ScalafmtConfig.default)
  implicit val styleReader: ConfDecoder[BaseStyle] =
    ConfDecoder.instance[BaseStyle] {
      case any =>
        ConfDecoder.stringConfDecoder.read(any).andThen { str =>
          val style =
            ScalafmtConfig.availableStyles.get(str.toLowerCase) match {
              case Some(s) => Ok(s)
              case None =>
                ConfError
                  .message(
                    s"Unknown style name $str. Expected one of ${ScalafmtConfig.activeStyles.keys}"
                  )
                  .notOk
            }
          style.map(BaseStyle.apply)
        }
    }
}
