package org.scalafmt

import scala.annotation.tailrec
import scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.scalafmt.config.FormatEvent.CompleteFormat
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.Split
import org.scalafmt.internal.State
import org.scalafmt.util.LoggerOps

/** (ugly) Utility to collect data about formatter.
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
        .groupBy(_.fileLine.line.value)
        .toSeq
        .sortBy(-_._2.size)
        .iterator
        .take(3)
        .foreach { case (line, group) =>
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

    val toks = if (null == formatOps) null else formatOps.tokens.arr
    if (null != toks) {
      if (null != formatTokenExplored)
        formatTokenExplored.zipWithIndex.sortBy(-_._1).take(3).foreach {
          case (visits, idx) =>
            LoggerOps.logger.debug("Visited " + toks(idx) + ": " + visits)
        }

      val stack = new mutable.ListBuffer[String]
      val posWidth = s"%${1 + math.log10(toks.last.left.end).toInt}d"
      @tailrec
      def iter(state: State): Unit =
        if (state.prev ne State.start) {
          val prev = state.prev
          val tok = toks(prev.depth).left
          val clean = LoggerOps.cleanup(tok).slice(0, 15).formatted("%-15s")
          stack.prepend(
            s"${tok.end.formatted(posWidth)}: $clean" +
              s" ${state.split} ${prev.indentation} ${prev.column} [${state.cost}]"
          )
          iter(prev)
        }
      if (state ne State.start) {
        iter(state)
        stack.foreach(LoggerOps.logger.debug)
      }
    }

    LoggerOps.logger.debug(s"Total cost: ${state.cost}")
  }

}

object Debug {

  var explored = 0

  def ns2ms(nanoseconds: Long): Long =
    TimeUnit.MILLISECONDS.convert(nanoseconds, TimeUnit.NANOSECONDS)

}
