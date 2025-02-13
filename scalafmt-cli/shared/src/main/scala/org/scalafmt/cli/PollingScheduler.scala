package org.scalafmt.cli

object PollingScheduler {
  trait Cancelable {
    def cancel(): Unit
  }
}

trait PollingScheduler {

  /** Start polling at the given interval. The function is executed *
    * periodically.
    */
  def start(intervalMs: Int)(fn: () => Unit): PollingScheduler.Cancelable
}
