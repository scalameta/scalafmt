package org.scalafmt.cli

import scala.scalajs.js.timers._

class PlatformPollingScheduler extends PollingScheduler {
  override def start(
      intervalMs: Int,
  )(fn: () => Unit): PollingScheduler.Cancelable = {
    val handle = setInterval(intervalMs)(fn())
    () => clearInterval(handle)
  }
}
