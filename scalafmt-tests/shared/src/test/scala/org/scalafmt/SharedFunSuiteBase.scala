package org.scalafmt

import scala.concurrent.duration._

import munit.diff.DiffOptions

trait SharedFunSuiteBase extends munit.FunSuite {

  implicit val diffOptions: DiffOptions = DiffOptions.withShowLines(true)
    .withContextSize(10)

  // munit's default is 30s per test; a few heavy format cases exceed that on
  // Native/JS, which lack a JIT and run several times slower than the JVM.
  override def munitTimeout: Duration = 2.minutes

}
