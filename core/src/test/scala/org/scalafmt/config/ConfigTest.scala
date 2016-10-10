package org.scalafmt.config

import org.scalafmt.util.logger
import org.scalatest.FunSuite

class ConfigTest extends FunSuite {
  test("displayAll") {
    val output =
      Config.toHocon(ScalafmtConfig.default.fields).mkString("\n")
    logger.elem(output)
  }

}
