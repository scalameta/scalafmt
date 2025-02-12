package org.scalafmt.cli

import java.util.concurrent._

class PlatformPollingScheduler extends PollingScheduler {
  private val scheduler: ScheduledExecutorService = Executors
    .newScheduledThreadPool(1)

  override def start(
      intervalMs: Int,
  )(fn: () => Unit): PollingScheduler.Cancelable = {
    val task = new Runnable {
      def run(): Unit = fn()
    }
    val future = scheduler
      .scheduleAtFixedRate(task, 0, intervalMs.toLong, TimeUnit.MILLISECONDS)
    () => future.cancel(false)
  }
}
