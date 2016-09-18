package org.scalafmt

import com.typesafe.config.ConfigFactory
import org.scalafmt.hocon.Hocon2Class

object Config {

  def fromHocon(string: String) =
    Hocon2Class.gimmeClass[ScalafmtStyle](string, ScalafmtStyle.configReader)

}
