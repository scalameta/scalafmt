package org.scalafmt.config

import metaconfig.Result
import org.scalafmt.config.hocon.Hocon2Class

object Config {

  def fromHocon(string: String,
                path: Option[String] = None): Result[ScalafmtConfig] =
    Hocon2Class
      .gimmeClass[ScalafmtConfig](string, ScalafmtConfig.configReader, path)

}
