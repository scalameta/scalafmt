package org.scalafmt.util

import scala.collection.mutable

object ValidationOps {
  def assertNonNegative(ns: sourcecode.Text[Int]*): Unit = {
    ns.foreach { n =>
      if (n.value < 0)
        throw new IllegalArgumentException(
          s"${n.source} must be non-negative, was ${n.value}"
        )
    }
  }

  def addIf(
      what: sourcecode.Text[Boolean],
      why: => String = ""
  )(implicit errors: mutable.Buffer[String]): Unit =
    if (what.value) errors += what.source + why

}
