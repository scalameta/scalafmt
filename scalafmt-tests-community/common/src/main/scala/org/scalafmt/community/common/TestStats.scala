package org.scalafmt.community.common

case class TestStats(
    checkedFiles: Int,
    errors: Int,
    lastError: Option[Throwable],
    timeTaken: Long,
    linesParsed: Int,
)

object TestStats {
  final val init = TestStats(0, 0, None, 0, 0)

  def merge(s1: TestStats, s2: TestStats): TestStats = TestStats(
    s1.checkedFiles + s2.checkedFiles,
    s1.errors + s2.errors,
    s1.lastError.orElse(s2.lastError),
    s1.timeTaken + s2.timeTaken,
    s1.linesParsed + s2.linesParsed,
  )

  case class Style(expectedStatesVisited: Int)
}
