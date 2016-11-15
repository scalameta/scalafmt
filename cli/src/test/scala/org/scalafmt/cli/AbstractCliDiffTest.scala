package org.scalafmt.cli

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.LoggerOps

abstract class AbstractCliDiffTest
    extends AbstractCliTest
    with DiffAssertions {
  import FileTestOps._

  def skip(original: String, expected: String, diff: String): Unit =
    ignore(LoggerOps.reveal(original)) { () }

  def check(original: String, expected: String, diff: String): Unit = {
    test(LoggerOps.reveal(original)) {
      val root = string2dir(original)
      val init = getMockOptions(root)
      val bais = new ByteArrayInputStream(diff.getBytes)
      val baos = new ByteArrayOutputStream()
      val config = Cli.getConfig(Array("--diff", "--stdin"), init).get
      Cli.run(
        config.copy(
          common = config.common.copy(
            in = bais
          )
        ))
      val obtained = dir2string(root)
      assertNoDiff(obtained, expected)
    }
  }
}
