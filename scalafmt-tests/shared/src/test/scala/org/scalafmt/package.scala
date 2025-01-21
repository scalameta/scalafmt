package org

import munit.diff.DiffOptions

package object scalafmt {
  implicit val diffOptions: DiffOptions = DiffOptions.withShowLines(true)
    .withContextSize(10)
}
