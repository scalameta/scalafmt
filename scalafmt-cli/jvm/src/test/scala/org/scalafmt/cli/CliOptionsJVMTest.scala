package org.scalafmt.cli

import FileTestOps._
import munit.FunSuite

class CliOptionsJVMTest extends FunSuite {

  private val baseCliOptionsWithOut = baseCliOptions
    .copy(common = baseCliOptions.common.copy(out = System.out))

  Seq("--stdin", "--stdout").foreach { arg =>
    test(s"don't write info when using $arg") {
      val options = Cli.getConfig(baseCliOptionsWithOut, arg).get
      val cons = System.console()
      if (cons ne null) options.common.info match {
        case x: Output.FromWriter if x.obj eq cons.writer() =>
        case x => fail(s"info should be writing to console: $x")
      }
      else options.common.info match {
        case x: Output.FromStream if x.obj eq Output.NoopStream.printStream =>
        case x => fail(s"info should be writing to NoopStream: $x")
      }
    }
  }
}
