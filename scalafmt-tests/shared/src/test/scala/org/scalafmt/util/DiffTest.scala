package org.scalafmt.util

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.PlatformCompat
import org.scalafmt.tests.BuildInfo

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
  val file = PlatformCompat
    .relativize(AbsoluteFile(DiffTest.testDir), AbsoluteFile(loc.path))
  val fullName = s"$file:${loc.line}: $name"
}

object DiffTest {
  val testDir = BuildInfo.resourceDirectory
}
