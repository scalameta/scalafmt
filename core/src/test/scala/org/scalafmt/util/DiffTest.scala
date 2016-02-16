package org.scalafmt.util

import org.scalafmt.ScalaStyle

case class DiffTest(spec: String, name: String, filename: String,
                    original: String, expected: String, skip: Boolean, only: Boolean,
                    style: ScalaStyle) {
  val fullName = s"$spec: $name"
}
