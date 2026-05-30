package org.scalafmt

import scala.annotation.nowarn
import scala.concurrent._

object TestPlatformCompat {
  @nowarn
  def executeAndWait(body: => Unit)(timeoutSeconds: Option[Int]): Unit =
    // we don't wait in scala.js
    Future(body)(scala.scalajs.concurrent.JSExecutionContext.queue)
}
