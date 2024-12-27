package org.scalafmt.dynamic

import scala.util.Try

case class ScalafmtVersion(
    major: Int,
    minor: Int,
    patch: Int,
    rc: Int = 0,
    snapshot: String = "",
) extends Comparable[ScalafmtVersion] {
  private val integerRepr: Int = {
    val max = 100
    val rcval = (rc + max - 1) % max // 0 -> 99, everything else is less
    Seq(major, minor, patch, rcval).reduce(_ * max + _)
  }

  def <(other: ScalafmtVersion): Boolean = compareTo(other) < 0

  def >(other: ScalafmtVersion): Boolean = other < this

  override def toString: String = s"$major.$minor.$patch" +
    (if (rc > 0) s"-RC$rc" else "") + snapshot

  override def compareTo(o: ScalafmtVersion): Int = {
    val cmp = Integer.compare(integerRepr, o.integerRepr)
    if (cmp != 0) cmp else snapshot.compareTo(o.snapshot)
  }
}

object ScalafmtVersion {

  private val versionRegex =
    """(\d{1,2})\.(\d{1,2})\.(\d{1,2})(?:-RC(\d{1,2}))?([-+].+)?""".r

  def parse(version: String): Option[ScalafmtVersion] = version match {
    case versionRegex(major, minor, patch, rc, snapshot) => Try(ScalafmtVersion(
        positiveInt(major),
        positiveInt(minor),
        positiveInt(patch),
        if (rc == null) 0 else positiveInt(rc),
        if (snapshot == null) "" else snapshot,
      )).toOption
    case _ => None
  }

  private def positiveInt(s: String): Int = {
    val i = s.toInt
    require(i >= 0)
    i
  }
}
