package tests

import org.scalatest.FunSuite
import org.scalafmt.dynamic.ScalafmtVersion
import org.scalafmt.dynamic.ScalafmtVersion.InvalidVersionException

class ScalafmtVersionSuite extends FunSuite {
  test("parse valid versions") {
    assert(ScalafmtVersion.parse("2.0.0") == Right(ScalafmtVersion(2, 0, 0, 0)))
    assert(ScalafmtVersion.parse("0.1.3") == Right(ScalafmtVersion(0, 1, 3, 0)))
    assert(ScalafmtVersion.parse("2.0.0-RC4") == Right(ScalafmtVersion(2, 0, 0, 4)))
    assert(ScalafmtVersion.parse("2.1.1") == Right(ScalafmtVersion(2, 1, 1, 0)))
  }

  test("fail on invalid versions") {
    assert(ScalafmtVersion.parse("2.0") == Left(InvalidVersionException("2.0")))
    assert(ScalafmtVersion.parse("v2.0.0") == Left(InvalidVersionException("v2.0.0")))
    assert(ScalafmtVersion.parse("avs") == Left(InvalidVersionException("avs")))
    assert(ScalafmtVersion.parse("1.2.3-M14") == Left(InvalidVersionException("1.2.3-M14")))
  }

  test("order versions") {
    assert(ScalafmtVersion(2, 0, 0, 0) > ScalafmtVersion(1, 5, 1, 0))
    assert(ScalafmtVersion(0, 1, 2, 0) > ScalafmtVersion(0, 1, 1, 0))
    assert(ScalafmtVersion(0, 2, 2, 0) > ScalafmtVersion(0, 1, 2, 0))
    assert(ScalafmtVersion(2, 0, 0, 0) < ScalafmtVersion(2, 0, 0, 4))
    assert(ScalafmtVersion(0, 1, 2, 0) < ScalafmtVersion(1, 0, 0, 0))
    assert(ScalafmtVersion(0, 1, 8, 0) < ScalafmtVersion(0, 2, 2, 0))
    // check false positives
    assert(!(ScalafmtVersion(2, 0, 0, 0) < ScalafmtVersion(1, 5, 1, 0)))
    assert(!(ScalafmtVersion(0, 1, 8, 0) > ScalafmtVersion(0, 2, 2, 0)))
  }
}
