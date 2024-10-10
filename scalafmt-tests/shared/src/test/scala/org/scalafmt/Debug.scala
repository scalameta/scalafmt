package org.scalafmt

import org.scalafmt.config.FormatEvent._
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter
import org.scalafmt.internal.Split
import org.scalafmt.internal.State
import org.scalafmt.util.LoggerOps

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.collection.mutable

/** (ugly) Utility to collect data about formatter.
  *
  * Only used during development.
  */
class Debug(val verbose: Boolean) {

  val enqueuedSplits = mutable.Set.empty[Split]
  var formatOps: FormatOps = _
  var routes: IndexedSeq[Seq[Split]] = _
  var completedEvent: Option[CompleteFormat] = None
  var locations: FormatWriter#FormatLocations = _
  val startTime = System.nanoTime()

  def elapsedNs = System.nanoTime() - startTime

  def enqueued(split: Split): Unit = if (verbose) enqueuedSplits += split

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
      enqueuedSplits.groupBy(_.fileLine.line.value).toSeq.sortBy(-_._2.size)
        .iterator.take(5).foreach { case (line, group) =>
          sb.append("Split(line=").append(line).append(" count=")
            .append(group.size).append("=")
          group.foreach(sb.append("\n\t").append(_))
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
        completedEvent.best.values.toSeq.sortBy(_.depth).take(5).foreach {
          state => sb.append("\n\t").append(log(state))
        }
        sb.append("\n")
        logger.debug(sb.toString())
      }

      if (null ne routes) {
        var tokidx = 0
        while (tokidx < toks.length) {
          logger.debug(s"FT: ${log2(toks(tokidx))}")
          routes(tokidx).foreach(s => logger.debug(s"> S: ${log(s)}"))
          tokidx += 1
        }
      }

      val stack = new mutable.ListBuffer[String]
      val posWidth = s"%${1 + math.log10(toks.last.left.end).toInt}d"
      @tailrec
      def iter(state: State): Unit = if (state.prev ne State.start) {
        val prev = state.prev
        val idx = prev.depth
        val tok = toks(idx).left
        val clean = "%-15s".format(cleanup(tok).slice(0, 15))
        stack.prepend(
          s"[$idx] ${posWidth.format(tok.end)}: $clean" +
            s" ${state.split} ${prev.indentation} ${prev.column} [${state.cost}]",
        )
        iter(prev)
      }
      val finalState = completedEvent.finalState
      if (null != finalState) {
        if (finalState ne State.start) iter(finalState)
        stack.foreach(logger.debug)
        logger.debug(s"Total cost: ${finalState.cost}")
      }
    }
  }

  def ns2ms(nanoseconds: Long): Long = TimeUnit.MILLISECONDS
    .convert(nanoseconds, TimeUnit.NANOSECONDS)

}
