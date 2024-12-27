package org.scalafmt.util

import scala.collection.mutable

object ValidationOps {

  def checkNonNeg(
      ns: sourcecode.Text[Int]*,
  )(implicit errors: mutable.Buffer[String]): Unit = ns.foreach(n =>
    if (n.value < 0) errors +=
      s"${n.source} must be non-negative, was ${n.value}",
  )

  def checkPositive(
      ns: sourcecode.Text[Int]*,
  )(implicit errors: mutable.Buffer[String]): Unit = ns.foreach(n =>
    if (n.value <= 0) errors += s"${n.source} must be positive, was ${n.value}",
  )

  def checkNonNegOpt(
      ns: sourcecode.Text[Option[Int]]*,
  )(implicit errors: mutable.Buffer[String]): Unit = ns.foreach(n =>
    n.value.foreach(nv =>
      if (nv < 0) errors += s"${n.source} must be non-negative, was $nv",
    ),
  )

  def checkPositiveOpt(
      ns: sourcecode.Text[Option[Int]]*,
  )(implicit errors: mutable.Buffer[String]): Unit = ns.foreach(n =>
    n.value.foreach(nv =>
      if (nv <= 0) errors += s"${n.source} must be positive, was $nv",
    ),
  )

  def addIf(what: sourcecode.Text[Boolean])(implicit
      errors: mutable.Buffer[String],
  ): Unit = addIfDirect(what.value, what.source)

  def addIf(what: sourcecode.Text[Boolean], why: => String)(implicit
      errors: mutable.Buffer[String],
  ): Unit = addIfDirect(what.value, s"$why: ${what.source}")

  def addIfDirect(what: Boolean, why: => String)(implicit
      errors: mutable.Buffer[String],
  ): Unit = if (what) errors += why

}
