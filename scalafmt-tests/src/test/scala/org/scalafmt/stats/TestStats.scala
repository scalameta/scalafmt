package org.scalafmt.stats

import java.util.Date

import org.scalafmt.util.Result

case class TestStats(
    createdAt: Long,
    results: Seq[Result],
    javaInfo: JavaInfo,
    osInfo: OsInfo,
    runtimeInfo: RuntimeInfo,
    gitInfo: GitInfo
) {

  def shortInfo: String =
    s"TestStats(created=${new Date(createdAt)}," + s"commit=${gitInfo.commit})"

  def shortCommit: String = gitInfo.commit.take(6)

  def intersectResults(other: TestStats): Seq[(Result, Result)] = {
    val resultByName = other.results.map(x => x.test.fullName -> x).toMap
    results.collect {
      case r if resultByName.contains(r.test.fullName) =>
        r -> resultByName(r.test.fullName)
    }
  }
}

object TestStats {

  def apply(results: Seq[Result]): TestStats =
    TestStats(
      System.currentTimeMillis(),
      results,
      JavaInfo(),
      OsInfo(),
      RuntimeInfo(),
      GitInfo()
    )
}
