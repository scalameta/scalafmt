package org.scalafmt

/**
  * Single source of truth for version number.
  */
object Versions {
  // Nightly, used in CLI, build.sbt, etc.
  val nightly = "0.3.0-RC1"
  // Stable, used in official user docs.
  val stable = nightly
  val scala = "2.11.8"
}
