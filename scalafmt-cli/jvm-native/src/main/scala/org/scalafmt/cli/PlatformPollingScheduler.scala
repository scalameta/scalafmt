package org.scalafmt.cli

import java.util.concurrent._

class PlatformPollingScheduler extends PollingScheduler {
  // daemon: the progress-display poller must never keep the JVM alive (the
  // final render happens synchronously in TermDisplay.end)
  private val scheduler: ScheduledExecutorService = Executors
    .newScheduledThreadPool(
      1,
      (r: Runnable) => {
        val t = new Thread(r, "scalafmt-term-display")
        t.setDaemon(true)
        t
      },
    )

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
