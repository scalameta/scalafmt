package org.scalafmt

import scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.scalafmt.config.FormatEvent.CompleteFormat
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.Split
import org.scalafmt.internal.State

/**
  * (ugly) Utility to collect data about formatter.
  *
  * Only used during development.
  */
object Debug {

  var formatTokenExplored: IndexedSeq[Int] = _
  val enqueuedSplits = mutable.Set.empty[Split]
  var formatOps: FormatOps = _
  var lastTestExplored = 0
  var explored = 0
  var state = State.start
  def tokens = formatOps.tokens.arr
  var startTime = System.nanoTime()

  def newTest(): Unit = {
    startTime = System.nanoTime()
    lastTestExplored = explored
  }

  def ns2ms(nanoseconds: Long): Long =
    TimeUnit.MILLISECONDS.convert(nanoseconds, TimeUnit.NANOSECONDS)

  def elapsedNs = System.nanoTime() - startTime

  def exploredInTest = explored - lastTestExplored

  def enqueued(split: Split): Unit = {
    enqueuedSplits += split
  }

  def completed(event: CompleteFormat): Unit = {
    explored += event.totalExplored
    Debug.explored += event.totalExplored
    formatTokenExplored = event.visits
    state = event.finalState
  }

}
