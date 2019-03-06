package org.scalafmt.util

object ValidationOps {
  def assertNonNegative(ns: sourcecode.Text[Int]*): Unit = {
    ns.foreach { n =>
      if (n.value < 0)
        throw new IllegalArgumentException(
          s"${n.source} must be non-negative, was ${n.value}"
        )
    }
  }
}
