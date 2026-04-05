package org.scalafmt.config

import munit.FunSuite

class FileHeaderConfigTest extends FunSuite {

  test("fileOverride correctly merges fileHeader per glob") {
    val cfg = ScalafmtConfig.fromHoconString(
      """|fileHeader {
         |  license = Apache-2.0
         |  copyrightHolder = "Org"
         |  since = 2020
         |}
         |fileOverride {
         |  "glob:**.sbt" {
         |    fileHeader.style = line
         |  }
         |  "glob:**/test/**" {
         |    fileHeader.text = "Test only"
         |  }
         |}
         |""".stripMargin,
    ).get

    val sbtCfg = cfg.getConfigFor("build.sbt").get
    assert(sbtCfg.fileHeader.style eq FileHeader.Style.line)
    assertEquals(sbtCfg.fileHeader.license, Some(License.`Apache-2.0`))

    val testCfg = cfg.getConfigFor("src/test/Foo.scala").get
    assertEquals(testCfg.fileHeader.text, Some("Test only"))

    val mainCfg = cfg.getConfigFor("src/main/Foo.scala").get
    assert(mainCfg.fileHeader.style eq FileHeader.Style.block)
    assertEquals(mainCfg.fileHeader.license, Some(License.`Apache-2.0`))
  }

  test("shortcut fileHeader = \"text\" in override replaces entire FileHeader") {
    val cfg = ScalafmtConfig.fromHoconString(
      """|fileHeader {
         |  license = Apache-2.0
         |  copyrightHolder = "Org"
         |}
         |fileOverride {
         |  "glob:**.sbt" {
         |    fileHeader = "SBT header"
         |  }
         |}
         |""".stripMargin,
    ).get

    val sbtCfg = cfg.getConfigFor("build.sbt").get
    assertEquals(sbtCfg.fileHeader.text, Some("SBT header"))
    // The shortcut replaces the whole FileHeader, so license should be gone
    assertEquals(sbtCfg.fileHeader.license, None)
  }

  test("fileHeader = none in override disables header for matched files") {
    val cfg = ScalafmtConfig.fromHoconString(
      """|fileHeader {
         |  license = MIT
         |  copyrightHolder = "Org"
         |}
         |fileOverride {
         |  "glob:**/generated/**" {
         |    fileHeader = none
         |  }
         |}
         |""".stripMargin,
    ).get

    val genCfg = cfg.getConfigFor("src/generated/Foo.scala").get
    assert(!genCfg.fileHeader.isActive)

    val mainCfg = cfg.getConfigFor("src/main/Foo.scala").get
    assert(mainCfg.fileHeader.isActive)
  }

  test("all 12 SPDX identifiers are accepted") {
    val ids = Seq(
      "Apache-2.0", "MIT", "BSD-2-Clause", "BSD-3-Clause",
      "MPL-2.0", "GPL-2.0-only", "GPL-3.0-only",
      "LGPL-2.1-only", "LGPL-3.0-only",
      "EPL-2.0", "ISC", "Unlicense",
    )
    ids.foreach { id =>
      val cfg = ScalafmtConfig.fromHoconString(
        s"""fileHeader.license = "$id" """,
      )
      assert(cfg.isOk, s"License $id should be accepted: ${cfg.toEither}")
    }
  }

  test("comment.width < 20 - config error") {
    val result = ScalafmtConfig.fromHoconString(
      """|fileHeader {
         |  text = "hi"
         |  comment.width = 10
         |}
         |""".stripMargin,
    )
    assert(result.isNotOk, "width < 20 should fail")
  }

  test("year out of range - config error") {
    val result = ScalafmtConfig.fromHoconString(
      "fileHeader { text = \"hi\", year = 100 }",
    )
    assert(result.isNotOk, "year = 100 should fail")
  }
}
