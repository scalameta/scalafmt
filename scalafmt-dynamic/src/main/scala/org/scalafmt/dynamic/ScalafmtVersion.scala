package org.scalafmt.dynamic

import scala.util.control.{NonFatal, NoStackTrace}

case class ScalafmtVersion(major: Int, minor: Int, patch: Int, rc: Int) {
  private val integerRepr: Int =
    major * 100 + minor * 10 + patch

  def <(other: ScalafmtVersion): Boolean =
    if (integerRepr == other.integerRepr)
      rc != 0 && (other.rc == 0 || rc < other.rc)
    else integerRepr < other.integerRepr

  def >(other: ScalafmtVersion): Boolean = this != other && !(this < other)

  override def toString: String =
    s"$major.$minor.$patch" + (if (rc > 0) s"-RC$rc" else "")
}

object ScalafmtVersion {
  case class InvalidVersionException(version: String)
      extends Exception(s"Invalid scalafmt version $version")
      with NoStackTrace

  private val versionRegex = """(\d)\.(\d)\.(\d)(-RC(\d))?""".r

  def parse(version: String): Either[InvalidVersionException, ScalafmtVersion] =
    try {
      version match {
        case versionRegex(major, minor, patch, null, null) =>
          Right(
            ScalafmtVersion(
              positiveInt(major),
              positiveInt(minor),
              positiveInt(patch),
              0
            )
          )
        case versionRegex(major, minor, patch, _, rc) =>
          Right(
            ScalafmtVersion(
              positiveInt(major),
              positiveInt(minor),
              positiveInt(patch),
              positiveInt(rc)
            )
          )
        case _ => Left(InvalidVersionException(version))
      }
    } catch {
      case e if NonFatal(e) => Left(InvalidVersionException(version))
    }

  private def positiveInt(s: String): Int = {
    val i = s.toInt
    require(i >= 0)
    i
  }
}
