package org.scalafmt.stats

case class RuntimeInfo(
    availableProcessor: Int,
    maxMemory: Long,
    totalMemory: Long
)

object RuntimeInfo {

  def apply(): RuntimeInfo = {
    val runtime = Runtime.getRuntime
    RuntimeInfo(
      runtime.availableProcessors(),
      runtime.maxMemory(),
      runtime.totalMemory()
    )
  }
}
