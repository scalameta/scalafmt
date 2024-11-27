package org.scalafmt

import org.scalafmt.config.LineEndings
import org.scalafmt.config.ScalafmtConfig

import scala.meta.internal.prettyprinters.DoubleQuotes

import java.lang.System.lineSeparator

import munit.FunSuite

class EmptyFileTest extends FunSuite {

  private val cfgWithLineEndingsCRLF = ScalafmtConfig.default
    .withLineEndings(LineEndings.windows)
  private val cfgWithLineEndingsKeep = ScalafmtConfig.default
    .withLineEndings(LineEndings.preserve)

  Seq(
    ("", "\n", "\r\n", "\n"),
    ("  \n  \n  ", "\n", "\r\n", "\n"),
    ("  \r\n  \r\n  ", "\n", "\r\n", "\r\n"),
    (lineSeparator(), "\n", "\r\n", lineSeparator()),
    (s"   $lineSeparator  ", "\n", "\r\n", lineSeparator()),
  ).foreach { case (original, expectedUnix, expectedCrlf, expectedKeep) =>
    defineTest(original, expectedUnix, ScalafmtConfig.default, "unix")
    defineTest(original, expectedCrlf, cfgWithLineEndingsCRLF, "crlf")
    defineTest(original, expectedKeep, cfgWithLineEndingsKeep, "keep")
  }

  private def defineTest(
      original: String,
      expected: String,
      cfg: ScalafmtConfig,
      label: String,
  ): Unit = {
    val expectedQuoted = DoubleQuotes(expected)
    test(s"empty tree formats to newline [$label]: ${DoubleQuotes(
        original,
      )} -> $expectedQuoted") {
      val obtained = Scalafmt.format(original, cfg).get
      if (obtained != expected) fail(
        s"values are not equal: ${DoubleQuotes(obtained)} != $expectedQuoted",
      )
    }
  }

}
