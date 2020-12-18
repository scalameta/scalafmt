package org.scalafmt.dynamic

import munit.FunSuite

class ScalafmtVersionSuite extends FunSuite {
  test("parse valid versions") {
    assert(
      ScalafmtVersion.parse("2.0.0") == Some(ScalafmtVersion(2, 0, 0))
    )
    assert(
      ScalafmtVersion.parse("0.1.3") == Some(ScalafmtVersion(0, 1, 3))
    )
    assert(
      ScalafmtVersion.parse("2.0.0-RC4") == Some(ScalafmtVersion(2, 0, 0, 4))
    )
    assert(
      ScalafmtVersion.parse("2.1.1") == Some(ScalafmtVersion(2, 1, 1))
    )
    assert(
      ScalafmtVersion.parse("2.2.3-SNAPSHOT") ==
        Some(ScalafmtVersion(2, 2, 3, 0, true))
    )
    assert(
      ScalafmtVersion.parse("2.0.0-RC1-SNAPSHOT") ==
        Some(ScalafmtVersion(2, 0, 0, 1, true))
    )
    assert(
      ScalafmtVersion.parse("2.2.2-SNAPSHOT") ==
        Some(ScalafmtVersion(2, 2, 2, 0, true))
    )
  }

  test("toString") {
    assert(ScalafmtVersion.parse("2.2.2-RC2").get.toString == "2.2.2-RC2")
    assert(ScalafmtVersion.parse("2.2.2").get.toString == "2.2.2")
    assert(
      ScalafmtVersion
        .parse("2.2.2-SNAPSHOT")
        .get
        .toString == "2.2.2-SNAPSHOT"
    )
    assert(
      ScalafmtVersion
        .parse("2.2.2-RC2-SNAPSHOT")
        .get
        .toString == "2.2.2-RC2-SNAPSHOT"
    )
  }

  test("fail on invalid versions") {
    assert(ScalafmtVersion.parse("2.0") eq None)
    assert(ScalafmtVersion.parse("v2.0.0") eq None)
    assert(ScalafmtVersion.parse("avs") eq None)
    assert(ScalafmtVersion.parse("1.2.3-M14") eq None)
    assert(ScalafmtVersion.parse("1.1.1.1") eq None)
    assert(ScalafmtVersion.parse("2.-1.0") eq None)
    assert(ScalafmtVersion.parse("2.1.0.") eq None)
    assert(ScalafmtVersion.parse(",2.1.0") eq None)
    assert(ScalafmtVersion.parse("2.1a.0") eq None)
    assert(ScalafmtVersion.parse("2.1.0-") eq None)
    assert(ScalafmtVersion.parse("2.1.0-rc1") eq None)
    assert(ScalafmtVersion.parse("2.1.0-RC1-M4") eq None)
    assert(ScalafmtVersion.parse("2.0.0-RC1+metadata") eq None)
  }

  test("order versions") {
    assert(ScalafmtVersion(2, 0, 0, 0) > ScalafmtVersion(1, 5, 1, 0))
    assert(ScalafmtVersion(2, 0, 0, 0) > ScalafmtVersion(2, 0, 0, 1))
    assert(ScalafmtVersion(2, 0, 0, 4) > ScalafmtVersion(1, 9, 9, 9))
    assert(ScalafmtVersion(0, 1, 2, 0) > ScalafmtVersion(0, 1, 1, 0))
    assert(ScalafmtVersion(0, 2, 2, 0) > ScalafmtVersion(0, 1, 2, 0))
    assert(ScalafmtVersion(2, 0, 0, 2) < ScalafmtVersion(2, 0, 0, 4))
    assert(ScalafmtVersion(2, 0, 0, 2) < ScalafmtVersion(2, 0, 0, 0))
    assert(ScalafmtVersion(0, 1, 2, 0) < ScalafmtVersion(1, 0, 0, 0))
    assert(ScalafmtVersion(0, 1, 8, 0) < ScalafmtVersion(0, 2, 2, 0))
    assert(!(ScalafmtVersion(2, 0, 0, 0) < ScalafmtVersion(1, 5, 1, 0)))
    assert(!(ScalafmtVersion(0, 1, 8, 0) > ScalafmtVersion(0, 2, 2, 0)))
  }
}
