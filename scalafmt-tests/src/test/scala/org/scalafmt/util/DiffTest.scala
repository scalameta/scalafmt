package org.scalafmt.util

import org.scalactic.source.Position
import org.scalafmt.config.ScalafmtConfig

case class DiffTest(
    name: String,
    loc: Position,
    original: String,
    expected: String,
    skip: Boolean,
    only: Boolean,
    style: ScalafmtConfig
) {
  val fullName = s"${loc.fileName}:${loc.lineNumber}: $name"
}
