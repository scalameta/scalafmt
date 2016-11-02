package org.scalafmt.bootstrap

import org.scalatest.FunSuite

class BootstrapTest extends FunSuite {
  // ignored until scalafmt-cli is released with supporting.
  ignore("bootstrap works") {
    val original = "object   A     {    }"
    val expected = "object A {}\n\n"
    val Right(cli) = Scalafmt.fromVersion("0.4.9-RC3")
    val obtained = cli.format(original)
    assert(obtained === expected)
  }
}
