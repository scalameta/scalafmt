package org.scalafmt

/**
  * Single source of truth for version number.
  */
object Versions {
  // Nightly, used in CLI, build.sbt, etc.
  val nightly = "0.1.5"
  // Stable, used in official user docs.
  val stable = "0.1.4"
}
