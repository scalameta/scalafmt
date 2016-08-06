package org.scalafmt

/**
  * Single source of truth for version number.
  */
object Versions {
  // Nightly, used in CLI, build.sbt, etc.
  val nightly = "0.2.13-SNAPSHOT"
  // Stable, used in official user docs.
  val stable = "0.2.12"
  val scala = "2.11.8"
}
