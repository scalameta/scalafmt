package org.scalafmt.util

import scala.collection.mutable

object ValidationOps {

  def addIfNegative(
      ns: sourcecode.Text[Int]*
  )(implicit errors: mutable.Buffer[String]): Unit =
    ns.foreach { n =>
      if (n.value < 0)
        errors += s"${n.source} must be non-negative, was ${n.value}"
    }

  def addIf(
      what: sourcecode.Text[Boolean],
      why: => String = ""
  )(implicit errors: mutable.Buffer[String]): Unit =
    addIfDirect(what.value, what.source + why)

  def addIfDirect(
      what: Boolean,
      why: => String = ""
  )(implicit errors: mutable.Buffer[String]): Unit =
    if (what) errors += why

}
