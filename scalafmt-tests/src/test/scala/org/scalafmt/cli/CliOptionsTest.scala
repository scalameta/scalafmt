package org.scalafmt.cli

import scala.meta.dialects.Paradise211
import scala.meta.parsers.Parse

import metaconfig.Configured.NotOk
import metaconfig.Configured.Ok
import org.scalafmt
import org.scalafmt.config.AlignToken
import org.scalafmt.config.ImportSelectors
import org.scalafmt.config.IndentOperator
import org.scalafmt.config.ScalafmtConfig
import org.scalatest.FunSuite

class CliOptionsTest extends FunSuite {

  test("style = ...") {
    import org.scalafmt.config.Config
    val NotOk(err) = Config.fromHoconString("style = foobar")
    assert(
      "Unknown style name foobar. Expected one of: Scala.js, IntelliJ, default, defaultWithAlign" == err.msg
    )

    val overrideOne = Config.fromHoconString("""|style = defaultWithAlign
                                                |maxColumn = 100
                                                |""".stripMargin)
    assert(
      Ok(ScalafmtConfig.defaultWithAlign.copy(maxColumn = 100)) == overrideOne
    )
    assert(
      Ok(ScalafmtConfig.intellij) == Config.fromHoconString("style = intellij")
    )
    assert(
      Ok(ScalafmtConfig.scalaJs) == Config.fromHoconString("style = Scala.js")
    )
    assert(
      Ok(ScalafmtConfig.defaultWithAlign) == Config
        .fromHoconString("style = defaultWithAlign")
    )
  }

}
