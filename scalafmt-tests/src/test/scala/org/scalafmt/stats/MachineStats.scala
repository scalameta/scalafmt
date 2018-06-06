package org.scalafmt.stats

case class MachineStats(
    javaInfo: JavaInfo,
    osInfo: OsInfo,
    runtimeInfo: RuntimeInfo,
    gitInfo: GitInfo
)

object MachineStats {

  def apply(): MachineStats =
    MachineStats(
      JavaInfo(),
      OsInfo(),
      RuntimeInfo(),
      GitInfo()
    )
}
