package org.scalafmt

import scala.concurrent._

object TestPlatformCompat {
  def executeAndWait(body: => Unit)(timeoutSeconds: Option[Int]): Unit = {
    val future = Future(body)(ExecutionContext.Implicits.global)
    timeoutSeconds
      .foreach(x => Await.ready(future, duration.Duration(x, duration.SECONDS)))
  }
}
