package org.scalafmt.util

import java.util.concurrent.TimeUnit

case class Result(
    test: DiffTest,
    obtained: String,
    obtainedHtml: String,
    tokens: Seq[FormatOutput],
    maxVisitsOnSingleToken: Int,
    visitedStates: Int,
    timeNs: Long
) {

  def title = f"${test.name} (${timeMs}ms, $visitedStates states)"

  def timeMs = TimeUnit.MILLISECONDS.convert(timeNs, TimeUnit.NANOSECONDS)

  def statesPerMs: Long = visitedStates / timeMs
}
