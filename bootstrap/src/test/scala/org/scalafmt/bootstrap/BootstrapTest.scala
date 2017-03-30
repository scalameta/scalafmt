package org.scalafmt.bootstrap

import org.scalatest.FunSuite

class BootstrapTest extends FunSuite {
  // ignored until scalafmt-cli is released with supporting.
  ignore("bootstrap works") {
    val original = "object   A     {    }"
    val expected = "object A {}\n"
    val Right(cli) = ScalafmtBootstrap.fromVersion("0.6.6+19-173886f3")
    val obtained = cli.format(original)
    assert(obtained === expected)
  }
}
