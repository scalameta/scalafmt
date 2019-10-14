package org.scalafmt.dynamic

import scala.util.control.{NonFatal, NoStackTrace}

case class ScalafmtVersion(major: Int, minor: Int, patch: Int, rc: Int) {
  private lazy val integerRepr: Int =
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

  def parse(version: String): Either[InvalidVersionException, ScalafmtVersion] =
    try {
      val splitByDot = version.split("\\.")
      val major = splitByDot(0).toInt
      val minor = splitByDot(1).toInt
      val splitByHyphen = splitByDot(2).split("-")
      val patch = splitByHyphen(0).toInt
      val rc =
        if (splitByHyphen.size == 1) 0
        else if (splitByHyphen(1).startsWith("RC"))
          splitByHyphen(1).drop(2).toInt
        else throw InvalidVersionException(version)
      Right(ScalafmtVersion(major, minor, patch, rc))
    } catch {
      case e if NonFatal(e) => Left(InvalidVersionException(version))
    }
}
