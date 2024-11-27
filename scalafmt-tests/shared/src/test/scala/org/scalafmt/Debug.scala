package org.scalafmt

import org.scalafmt.config.FormatEvent._
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter
import org.scalafmt.internal.Split
import org.scalafmt.util.LoggerOps

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable

/** (ugly) Utility to collect data about formatter.
  *
  * Only used during development.
  */
class Debug(val verbose: Boolean) {

  val enqueuedSplits = mutable.Map.empty[Split, Int]
  var formatOps: FormatOps = _
  var routes: IndexedSeq[Seq[Split]] = _
  var completedEvent: Option[CompleteFormat] = None
  var locations: FormatWriter#FormatLocations = _
  val startTime = System.nanoTime()

  def elapsedNs = System.nanoTime() - startTime

  def enqueued(split: Split): Unit = if (verbose) enqueuedSplits
    .updateWith(split) {
      case None => Some(1)
      case Some(x) => Some(x + 1)
    }

  def completed(event: CompleteFormat): Unit = {
    completedEvent = Option(event)
    Debug.explored.getAndAdd(event.totalExplored)
  }

  def formatTokenExplored = completedEvent.map(_.visits)
  def explored = completedEvent.map(_.totalExplored)

  def printTest(): Unit = {
    if (!verbose) return

    // splits
    if (enqueuedSplits.nonEmpty) {
      val sb = new StringBuilder()
      val groups = enqueuedSplits.toSeq.groupBy(_._1.fileLine.toString).toSeq
        .sortBy(-_._2.length)
      val minSize = groups.lift(5).fold(0)(_._2.length)
      groups.takeWhile(_._2.length >= minSize).foreach { case (line, group) =>
        sb.append("Split(line=").append(line).append(" unique=")
          .append(group.length).append(" count=").append(group.map(_._2).sum)
        group.sortBy(-_._2)
          .foreach(x => sb.append("\n\t").append(x._2).append(' ').append(x._1))
        sb.append("\n")
      }
      LoggerOps.logger.debug(sb.toString())
    }

    completedEvent.foreach(x => Debug.printCompletedEvent(x, routes, formatOps))
  }

}

object Debug {

  val explored = new AtomicInteger(0)

  def printCompletedEvent(
      completedEvent: CompleteFormat,
      routes: IndexedSeq[Seq[Split]],
      formatOps: FormatOps,
  ): Unit = {
    val toks = if (null == formatOps) null else formatOps.tokens.arr
    if (null != toks) {
      import LoggerOps._

      if (null != completedEvent.visits) {
        val sb = new StringBuilder()
        sb.append("Visited ").append(completedEvent.totalExplored).append(":")
        completedEvent.visits.zipWithIndex.sortBy(-_._1).take(10).foreach {
          case (visits, idx) => sb.append("\n\t").append(visits).append(": ")
              .append(toks(idx))
        }
        sb.append("\n")
        logger.debug(sb.toString())
      }

      if (null != completedEvent.best) {
        val sb = new StringBuilder()
        sb.append("Best splits:")
        completedEvent.best.values.toSeq.sortBy(_.depth).take(5)
          .foreach(state => sb.append("\n\t").append(log(state)))
        sb.append("\n")
        logger.debug(sb.toString())
      }

      LoggerOps.logDebugRoutes(routes, formatOps.tokens)
      LoggerOps.logDebugStateStack(completedEvent.finalState, formatOps.tokens)
    }
  }

  def ns2ms(nanoseconds: Long): Long = TimeUnit.MILLISECONDS
    .convert(nanoseconds, TimeUnit.NANOSECONDS)

}
