package org.scalafmt.config

import metaconfig.Result
import org.scalafmt.ScalafmtStyle
import org.scalafmt.config.hocon.Hocon2Class

object Config {

  def fromHocon(string: String,
                path: Option[String] = None): Result[ScalafmtStyle] =
    Hocon2Class
      .gimmeClass[ScalafmtStyle](string, ScalafmtStyle.configReader, path)

}
