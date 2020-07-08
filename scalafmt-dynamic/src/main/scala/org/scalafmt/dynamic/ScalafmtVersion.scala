package org.scalafmt.dynamic

import scala.util.control.NonFatal

case class ScalafmtVersion(
    major: Int,
    minor: Int,
    patch: Int,
    rc: Int = 0,
    snapshot: Boolean = false
) {
  private val integerRepr: Int =
    major * 100 + minor * 10 + patch

  def <(other: ScalafmtVersion): Boolean =
    if (integerRepr == other.integerRepr)
      rc != 0 && (other.rc == 0 || rc < other.rc)
    else integerRepr < other.integerRepr

  def >(other: ScalafmtVersion): Boolean = other < this

  override def toString: String =
    s"$major.$minor.$patch" +
      (if (rc > 0) s"-RC$rc" else "") +
      (if (snapshot) "-SNAPSHOT" else "")
}

object ScalafmtVersion {

  private val versionRegex = """(\d)\.(\d)\.(\d)(-RC(\d))?(-SNAPSHOT)?""".r

  def parse(version: String): Option[ScalafmtVersion] =
    try {
      version match {
        case versionRegex(major, minor, patch, null, null, snapshot) =>
          Some(
            ScalafmtVersion(
              positiveInt(major),
              positiveInt(minor),
              positiveInt(patch),
              0,
              snapshot != null
            )
          )
        case versionRegex(major, minor, patch, _, rc, snapshot) =>
          Some(
            ScalafmtVersion(
              positiveInt(major),
              positiveInt(minor),
              positiveInt(patch),
              positiveInt(rc),
              snapshot != null
            )
          )
        case _ => None
      }
    } catch {
      case e if NonFatal(e) => None
    }

  private def positiveInt(s: String): Int = {
    val i = s.toInt
    require(i >= 0)
    i
  }
}
