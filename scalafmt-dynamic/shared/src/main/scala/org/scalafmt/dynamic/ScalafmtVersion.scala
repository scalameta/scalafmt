package org.scalafmt.dynamic

import scala.util.Try

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

  private val versionRegex = """(\d)\.(\d)\.(\d)(?:-RC(\d))?(-SNAPSHOT)?""".r

  def parse(version: String): Option[ScalafmtVersion] =
    version match {
      case versionRegex(major, minor, patch, rc, snapshot) =>
        Try {
          ScalafmtVersion(
            positiveInt(major),
            positiveInt(minor),
            positiveInt(patch),
            if (rc == null) 0 else positiveInt(rc),
            snapshot != null
          )
        }.toOption
      case _ => None
    }

  private def positiveInt(s: String): Int = {
    val i = s.toInt
    require(i >= 0)
    i
  }
}
