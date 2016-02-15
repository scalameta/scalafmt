package org.scalafmt

import java.util.concurrent.TimeUnit

// This must exist somewhere, couldn't find it though.

class Stopwatch(val t1: Long) {

  def elapsedMs =
    TimeUnit.MILLISECONDS.convert(elapsedNs, TimeUnit.NANOSECONDS)

  def elapsedNs = System.nanoTime() - t1
}

object Stopwatch { def apply() = new Stopwatch(System.nanoTime()) }