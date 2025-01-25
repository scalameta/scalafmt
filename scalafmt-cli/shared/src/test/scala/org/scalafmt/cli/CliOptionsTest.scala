package org.scalafmt.cli

import org.scalafmt.Versions
import org.scalafmt.config.ScalafmtConfig

import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Path
import java.nio.file.Paths

import FileTestOps._
import metaconfig.Configured
import munit.FunSuite

class CliOptionsTest extends FunSuite {

  private val baseCliOptionsWithOut = baseCliOptions
    .copy(common = baseCliOptions.common.copy(out = System.out))

  test("preset = ...") {
    assertEquals(
      ScalafmtConfig.fromHoconString("preset = foobar"),
      Configured.error(
        "Unknown style \"foobar\". Expected one of: " +
          "Scala.js, IntelliJ, default, defaultWithAlign",
      ),
    )

    assertEquals(
      ScalafmtConfig.fromHoconString(
        """|preset = defaultWithAlign
           |maxColumn = 100
           |""".stripMargin,
      ),
      Configured.ok(ScalafmtConfig.defaultWithAlign.copy(maxColumn = 100)),
    )
    assertEquals(
      ScalafmtConfig.fromHoconString("preset = intellij"),
      Configured.ok(ScalafmtConfig.intellij),
    )
    assertEquals(
      ScalafmtConfig.fromHoconString("preset = Scala.js"),
      Configured.ok(ScalafmtConfig.scalaJs),
    )
    assertEquals(
      ScalafmtConfig.fromHoconString("preset = defaultWithAlign"),
      Configured.ok(ScalafmtConfig.defaultWithAlign),
    )
  }

  test(".configPath returns a path to the temporary file that contains configuration specified by --config-str") {
    val expected = "foo bar"
    val path: Path = baseCliOptions
      .copy(configStr = Some(s"""{version="${Versions
          .version}", onTestFailure="$expected"}""")).configPath
    val config = ScalafmtConfig.fromHoconFile(path).get
    assertEquals(config.onTestFailure, expected)
  }

  test(".configPath returns path to specified configuration path") {
    val tempPath = Files.createTempFile(".scalafmt", ".conf")
    val opt = baseCliOptions.copy(config = Some(tempPath))
    assertEquals(opt.configPath, tempPath)
  }

  test(".configPath returns path to workingDirectory's .scalafmt.conf by default, if exists") {
    assertEquals(baseCliOptions.config, None)
    assertEquals(baseCliOptions.configStr, None)
    intercept[NoSuchFileException](baseCliOptions.configPath)
  }

  test(".scalafmtConfig returns the configuration encoded from configStr if configStr is exists") {
    val expected = "foo bar"
    val opt = baseCliOptions.copy(configStr = Some(s"""{version="${Versions
        .version}", onTestFailure="$expected"}"""))
    assertEquals(opt.scalafmtConfig.get.onTestFailure, expected)
  }

  test(".scalafmtConfig returns the configuration read from configuration file located on configPath") {
    val expected = "foo bar"
    val configPath = Files.createTempFile(".scalafmt", ".conf")
    val config =
      s"""|
          |version="${Versions.version}"
          |maxColumn=100
          |onTestFailure="$expected"
          |""".stripMargin
    Files.write(configPath, config.getBytes)

    val opt = baseCliOptions.copy(config = Some(configPath))
    assertEquals(opt.scalafmtConfig.get.onTestFailure, expected)
  }

  test(".scalafmtConfig returns default ScalafmtConfig if configuration file is missing") {
    val configDir = Files.createTempDirectory("temp-dir")
    val configPath = Paths.get(configDir.toString + "/.scalafmt.conf")
    val opt = baseCliOptions.copy(config = Some(configPath))
    assert(opt.scalafmtConfig.isInstanceOf[Configured.NotOk])
    val confError = opt.scalafmtConfig.asInstanceOf[Configured.NotOk].error
    assert(confError.cause.exists(_.isInstanceOf[NoSuchFileException]))
  }

  test(".scalafmtConfig returns Configured.NotOk for invalid configuration") {
    val expected = "foo bar"
    val opt = baseCliOptions.copy(configStr = Some(s"""{invalidKey="${Versions
        .version}", onTestFailure="$expected"}"""))
    assert(opt.scalafmtConfig.isNotOk)
  }

  test("write info to out if not writing to stdout") {
    val options = Cli.getConfig(Array.empty[String], baseCliOptionsWithOut).get
    assertEquals(options.common.info.printStream, System.out)
  }

}
