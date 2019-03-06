package org.scalafmt.util

import org.scalafmt.config.ScalafmtConfig

case class DiffTest(
    spec: String,
    name: String,
    filename: String,
    original: String,
    expected: String,
    skip: Boolean,
    only: Boolean,
    style: ScalafmtConfig
) {
  val fullName = s"$spec: $name"
}
