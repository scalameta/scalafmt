package org.scalafmt

import scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.scalafmt.config.FormatEvent.CompleteFormat
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.Split
import org.scalafmt.internal.State
import org.scalafmt.util.LoggerOps

/**
  * (ugly) Utility to collect data about formatter.
  *
  * Only used during development.
  */
class Debug(val verbose: Boolean) {

  var formatTokenExplored: IndexedSeq[Int] = _
  val enqueuedSplits = mutable.Set.empty[Split]
  var formatOps: FormatOps = _
  var explored = 0
  var state = State.start
  def tokens = formatOps.tokens.arr
  var startTime = System.nanoTime()

  def elapsedNs = System.nanoTime() - startTime

  def enqueued(split: Split): Unit = {
    if (verbose)
      enqueuedSplits += split
  }

  def completed(event: CompleteFormat): Unit = {
    explored += event.totalExplored
    Debug.explored += event.totalExplored
    formatTokenExplored = event.visits
    state = event.finalState
  }

  def printTest(): Unit = {
    if (!verbose) return

    // splits
    if (enqueuedSplits.nonEmpty) {
      val sb = new StringBuilder()
      enqueuedSplits
        .groupBy(_.line.value)
        .toSeq
        .sortBy(-_._2.size)
        .iterator
        .take(3)
        .foreach {
          case (line, group) =>
            sb.append("Split(line=")
              .append(line)
              .append(" count=")
              .append(group.size)
              .append("=")
            group.foreach(sb.append("\n\t").append(_))
            sb.append("\n")
        }
      LoggerOps.logger.debug(sb.toString())
    }
  }

}

object Debug {

  var explored = 0

  def ns2ms(nanoseconds: Long): Long =
    TimeUnit.MILLISECONDS.convert(nanoseconds, TimeUnit.NANOSECONDS)

}
