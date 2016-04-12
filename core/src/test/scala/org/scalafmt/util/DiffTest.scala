package org.scalafmt.util

import org.scalafmt.ScalafmtStyle

case class DiffTest(spec: String,
                    name: String,
                    filename: String,
                    original: String,
                    expected: String,
                    skip: Boolean,
                    only: Boolean,
                    style: ScalafmtStyle) {
  val fullName = s"$spec: $name"
}
