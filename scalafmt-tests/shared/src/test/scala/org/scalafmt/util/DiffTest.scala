package org.scalafmt.util

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.tests.BuildInfo

import java.nio.file.Paths

import munit.Location

case class DiffTest(
    name: String,
    filename: String,
    loc: Location,
    original: String,
    expected: String,
    skip: Boolean,
    only: Boolean,
    style: ScalafmtConfig,
    stateVisits: Option[Int] = None,
    stateVisits2: Option[Int] = None,
) {
  val file = DiffTest.testDir.relativize(Paths.get(loc.path)).toString
  val fullName = s"$file:${loc.line}: $name"
}

object DiffTest {
  val testDir = BuildInfo.resourceDirectory.toPath
}
