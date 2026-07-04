package org.scalafmt

import munit.diff.DiffOptions

trait SharedFunSuiteBase extends munit.FunSuite {

  implicit val diffOptions: DiffOptions = DiffOptions.withShowLines(true)
    .withContextSize(10)

}
