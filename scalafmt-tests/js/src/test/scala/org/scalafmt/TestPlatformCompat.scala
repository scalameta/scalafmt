package org.scalafmt

import scala.concurrent._

object TestPlatformCompat {
  def executeAndWait(body: => Unit)(timeoutSeconds: Option[Int]): Unit =
    // we don't wait in scala.js
    Future(body)(scala.scalajs.concurrent.JSExecutionContext.queue)
}
