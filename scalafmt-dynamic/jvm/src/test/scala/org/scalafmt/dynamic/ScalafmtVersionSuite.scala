package org.scalafmt.dynamic

import munit.FunSuite

class ScalafmtVersionSuite extends FunSuite {
  test("parse valid versions") {
    assertEquals(ScalafmtVersion.parse("2.0.0"), Some(ScalafmtVersion(2, 0, 0)))
    assertEquals(ScalafmtVersion.parse("0.1.3"), Some(ScalafmtVersion(0, 1, 3)))
    assertEquals(
      ScalafmtVersion.parse("2.0.0-RC4"),
      Some(ScalafmtVersion(2, 0, 0, 4)),
    )
    assertEquals(ScalafmtVersion.parse("2.1.1"), Some(ScalafmtVersion(2, 1, 1)))
    assertEquals(
      ScalafmtVersion.parse("2.2.3-SNAPSHOT"),
      Some(ScalafmtVersion(2, 2, 3, 0, "-SNAPSHOT")),
    )
    assertEquals(
      ScalafmtVersion.parse("2.0.0-RC1-SNAPSHOT"),
      Some(ScalafmtVersion(2, 0, 0, 1, "-SNAPSHOT")),
    )
    assertEquals(
      ScalafmtVersion.parse("2.2.2-SNAPSHOT"),
      Some(ScalafmtVersion(2, 2, 2, 0, "-SNAPSHOT")),
    )
    assertEquals(
      ScalafmtVersion.parse("99.99.99-RC99-SNAPSHOT"),
      Some(ScalafmtVersion(99, 99, 99, 99, "-SNAPSHOT")),
    )
    assertEquals(
      ScalafmtVersion.parse("99.99.99-RC99+foobar"),
      Some(ScalafmtVersion(99, 99, 99, 99, "+foobar")),
    )
    assertEquals(
      ScalafmtVersion.parse("99.99.99+foobar"),
      Some(ScalafmtVersion(99, 99, 99, 0, "+foobar")),
    )
    assertEquals(
      ScalafmtVersion.parse("1.2.3-M14"),
      Some(ScalafmtVersion(1, 2, 3, 0, "-M14")),
    )
    assertEquals(
      ScalafmtVersion.parse("2.1.0-RC1-M4"),
      Some(ScalafmtVersion(2, 1, 0, 1, "-M4")),
    )
    assertEquals(
      ScalafmtVersion.parse("2.1.0-rc1"),
      Some(ScalafmtVersion(2, 1, 0, 0, "-rc1")),
    )
  }

  test("toString") {
    assertEquals(ScalafmtVersion.parse("2.2.2-RC2").get.toString, "2.2.2-RC2")
    assertEquals(ScalafmtVersion.parse("2.2.2").get.toString, "2.2.2")
    assertEquals(
      ScalafmtVersion.parse("2.2.2-SNAPSHOT").get.toString,
      "2.2.2-SNAPSHOT",
    )
    assertEquals(
      ScalafmtVersion.parse("2.2.2-RC2-SNAPSHOT").get.toString,
      "2.2.2-RC2-SNAPSHOT",
    )
  }

  test("fail on invalid versions") {
    assertEquals(ScalafmtVersion.parse("2.0"), None)
    assertEquals(ScalafmtVersion.parse("v2.0.0"), None)
    assertEquals(ScalafmtVersion.parse("avs"), None)
    assertEquals(ScalafmtVersion.parse("1.1.1.1"), None)
    assertEquals(ScalafmtVersion.parse("2.-1.0"), None)
    assertEquals(ScalafmtVersion.parse("2.1.0."), None)
    assertEquals(ScalafmtVersion.parse(",2.1.0"), None)
    assertEquals(ScalafmtVersion.parse("2.1a.0"), None)
    assertEquals(ScalafmtVersion.parse("2.1.0-"), None)
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
