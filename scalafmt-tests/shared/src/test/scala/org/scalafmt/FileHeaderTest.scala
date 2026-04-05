package org.scalafmt

import org.scalafmt.config._

import metaconfig.Configured
import munit.FunSuite

class FileHeaderTest extends FunSuite {

  private def format(code: String, hocon: String): String = {
    val cfg = ScalafmtConfig.fromHoconString(hocon).get
    Scalafmt.format(code, cfg).get
  }

  private def configOk(hocon: String): ScalafmtConfig =
    ScalafmtConfig.fromHoconString(hocon).get

  private def configErr(hocon: String): String =
    ScalafmtConfig.fromHoconString(hocon) match {
      case Configured.NotOk(err) => err.msg
      case Configured.Ok(_)      => fail("expected config error"); ""
    }

  // --- Config parsing / validation ---

  test("HOCON: license config parses correctly") {
    val cfg = configOk(
      """|fileHeader {
         |  license = Apache-2.0
         |  copyrightHolder = "Org"
         |  since = 2020
         |}
         |""".stripMargin,
    )
    assertEquals(cfg.fileHeader.license, Some(License.`Apache-2.0`))
    assertEquals(cfg.fileHeader.copyrightHolder, Some("Org"))
    assertEquals(cfg.fileHeader.since, Some(2020))
    assert(cfg.fileHeader.isActive)
  }

  test("HOCON: fileHeader = none produces inactive FileHeader") {
    val cfg = configOk("fileHeader = none")
    assert(!cfg.fileHeader.isActive)
    assertEquals(cfg.fileHeader, FileHeader())
  }

  test("HOCON: fileHeader = \"Custom text\" produces text shortcut") {
    val cfg = configOk("""fileHeader = "Custom text" """)
    assertEquals(cfg.fileHeader.text, Some("Custom text"))
    assert(cfg.fileHeader.isActive)
  }

  test("HOCON: invalid license - config error") {
    val err = configErr("fileHeader { license = INVALID }")
    assert(err.contains("INVALID"), err)
  }

  test("HOCON: since > year - config error") {
    val err = configErr(
      "fileHeader { since = 2030, year = 2020, license = MIT }",
    )
    assert(err.contains("fileHeader.since must be <= fileHeader.year"), err)
  }

  test("fileOverride with different fileHeader.style per glob") {
    val cfg = configOk(
      """|fileHeader {
         |  license = MIT
         |  copyrightHolder = "Org"
         |}
         |fileOverride {
         |  "glob:**.sbt" {
         |    fileHeader.style = line
         |  }
         |}
         |""".stripMargin,
    )
    val sbtCfg = cfg.getConfigFor("build.sbt").get
    assert(sbtCfg.fileHeader.style eq FileHeader.Style.line)
    val scalaCfg = cfg.getConfigFor("src/Main.scala").get
    assert(scalaCfg.fileHeader.style eq FileHeader.Style.block)
  }

  test("cross-config: block style + comments.wrap = standalone - error") {
    val err = configErr(
      """|fileHeader {
         |  license = MIT
         |  copyrightHolder = "Org"
         |}
         |comments.wrap = standalone
         |""".stripMargin,
    )
    assert(err.contains("fileHeader"), err)
  }

  test("cross-config: line style + comments.wrap = standalone - accepted") {
    configOk(
      """|fileHeader {
         |  license = MIT
         |  copyrightHolder = "Org"
         |  style = line
         |}
         |comments.wrap = standalone
         |""".stripMargin,
    )
  }

  // --- Content resolution ---

  test("raw > text > license precedence") {
    val result = format(
      "object A\n",
      """|fileHeader {
         |  raw = "// raw header"
         |  text = "text header"
         |  license = MIT
         |  copyrightHolder = "Org"
         |}
         |""".stripMargin,
    )
    assert(result.startsWith("// raw header"), result)
  }

  test("text = empty string - strip mode (existing header removed)") {
    val code =
      """|/*
         | * Old header
         | */
         |
         |object A
         |""".stripMargin
    val result = format(code, """fileHeader.text = "" """)
    assert(!result.contains("Old header"), result)
    assert(result.contains("object A"), result)
  }

  test("license = Apache-2.0 with since and explicit year") {
    val cfg =
      """|fileHeader {
         |  license = Apache-2.0
         |  copyrightHolder = "Org"
         |  since = 2020
         |  year = 2025
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("Copyright 2020-2025 Org"), result)
    assert(result.contains("SPDX-License-Identifier: Apache-2.0"), result)
  }

  test("since = year - single year, no range") {
    val cfg =
      """|fileHeader {
         |  license = MIT
         |  copyrightHolder = "Org"
         |  since = 2025
         |  year = 2025
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("Copyright 2025 Org"), result)
    assert(!result.contains("2025-2025"), result)
  }

  test("license without copyrightHolder - SPDX line only") {
    val cfg = "fileHeader { license = MIT, year = 2025 }"
    val result = format("object A\n", cfg)
    assert(result.contains("SPDX-License-Identifier: MIT"), result)
    assert(!result.contains("Copyright"), result)
  }

  test("copyrightHolder without since or year - no year in copyright") {
    val cfg =
      """|fileHeader {
         |  license = MIT
         |  copyrightHolder = "Holder"
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("Copyright Holder"), result)
    // Both since and year are None, so yearStr = ""  - no year in copyright line
  }

  // --- Comment wrapping: block style ---

  test("block style: default SpaceAsterisk prefix") {
    val cfg =
      """|fileHeader {
         |  text = "line"
         |  style = block
         |  comment.style = SpaceAsterisk
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("/*\n * line\n */"), result)
  }

  test("block style: Asterisk prefix") {
    // At indent 0, FormatMlc normalizes leading * to " *", so Asterisk
    // and SpaceAsterisk produce the same formatted output.
    val cfg =
      """|fileHeader {
         |  text = "line"
         |  style = block
         |  comment.style = Asterisk
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("/*"), result)
    assert(result.contains("line"), result)
    assert(result.contains("*/"), result)
  }

  test("block style: blankFirstLine = unfold (default, content on next line)") {
    val cfg =
      """|fileHeader {
         |  text = "line"
         |  style = block
         |  comment.blankFirstLine = unfold
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    // unfold = first line blank (no content on /* line)
    assert(result.contains("/*\n * line\n */"), result)
  }

  test("block style: blankFirstLine = fold (content on /* line)") {
    val cfg =
      """|fileHeader {
         |  text = "line"
         |  style = block
         |  comment.blankFirstLine = fold
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("/* line\n */"), result)
  }

  test("block style: blankLastLine = true") {
    val cfg =
      """|fileHeader {
         |  text = "line"
         |  style = block
         |  comment.blankLastLine = true
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("/*\n * line\n *\n */"), result)
  }

  // --- Comment wrapping: line style ---

  test("line style: single line") {
    val cfg =
      """|fileHeader {
         |  text = "line"
         |  style = line
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.startsWith("// line\n"), result)
  }

  test("line style: multi-line") {
    val cfg =
      """|fileHeader {
         |  text = "line1\nline2"
         |  style = line
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.startsWith("// line1\n// line2\n"), result)
  }

  // --- Comment wrapping: framed style ---

  test("framed style: all lines exactly width chars") {
    val cfg =
      """|fileHeader {
         |  text = "Hello World"
         |  style = framed
         |  comment.width = 40
         |}
         |comments.wrap = no
         |""".stripMargin
    val result = format("object A\n", cfg)
    val headerLines = result.linesIterator.takeWhile(_.contains("*")).toSeq
    headerLines.foreach { line =>
      assertEquals(line.length, 40, s"Line not 40 chars: '$line'")
    }
  }

  test("framed style: content is centered") {
    val cfg =
      """|fileHeader {
         |  text = "Hi"
         |  style = framed
         |  comment.width = 40
         |}
         |comments.wrap = no
         |""".stripMargin
    val result = format("object A\n", cfg)
    val contentLine = result.linesIterator
      .find(l => l.contains("Hi") && l.startsWith(" * ")).get
    assertEquals(contentLine.length, 40)
    // Verify centering: " * " prefix (3 chars) + leftPad + "Hi" should be well past col 3
    assert(contentLine.indexOf("Hi") > 5, s"Content should be centered: $contentLine")
  }

  test("framed style: blankFirstLine and blankLastLine") {
    val cfg =
      """|fileHeader {
         |  text = "Hi"
         |  style = framed
         |  comment.width = 40
         |  comment.blankFirstLine = unfold
         |  comment.blankLastLine = true
         |}
         |comments.wrap = no
         |""".stripMargin
    val result = format("object A\n", cfg)
    val headerLines = result.linesIterator.takeWhile(_.contains("*")).toSeq
    // top, blank, content, blank, bottom = 5 lines
    assertEquals(headerLines.length, 5, s"Expected 5 lines: $headerLines")
  }

  // --- File decomposition ---

  test("file with no comment - header inserted") {
    val result = format("object A\n", "fileHeader.text = \"header\"")
    assert(result.contains("header"), result)
    assert(result.contains("object A"), result)
  }

  test("file with block comment header - header replaced") {
    val code =
      """|/*
         | * Old
         | */
         |
         |object A
         |""".stripMargin
    val result = format(code, "fileHeader.text = \"New\"")
    assert(result.contains("New"), result)
    assert(!result.contains("Old"), result)
  }

  test("file with line comment header - header replaced") {
    val code =
      """|// Old header
         |
         |object A
         |""".stripMargin
    val result =
      format(code, """fileHeader { text = "New", style = line }""")
    assert(result.contains("// New"), result)
    assert(!result.contains("Old"), result)
  }

  test("shebang - preamble preserved, header after") {
    val code = "#!/usr/bin/env scala\nobject A\n"
    val result = format(
      code,
      """fileHeader { text = "header", style = line }""",
    )
    assert(result.startsWith("#!/usr/bin/env scala\n"), result)
    assert(result.contains("// header"), result)
  }

  test("scalafmt: directive - not treated as header") {
    val code =
      """|// scalafmt: { maxColumn = 120 }
         |object A
         |""".stripMargin
    val result = format(
      code,
      """fileHeader { text = "header", style = line }""",
    )
    assert(result.contains("// header"), result)
    assert(result.contains("scalafmt: { maxColumn = 120 }"), result)
  }

  test("@formatter:off - content returned unchanged") {
    val code =
      """|// @formatter:off
         |object A
         |""".stripMargin
    val result = format(
      code,
      """fileHeader { text = "header", style = line }""",
    )
    // Header should NOT be inserted
    assert(!result.contains("header"), result)
  }

  test("comment starting with 'format: off' prefix is not false positive") {
    // Verifies exact match: "format: official" should NOT trigger format-off
    val code =
      """|// format: official documentation
         |object A
         |""".stripMargin
    val result = format(
      code,
      """fileHeader { text = "header", style = line }""",
    )
    // format-off should NOT be triggered, so header IS inserted
    assert(result.contains("// header"), result)
  }

  test("scalafmt: directive then format: off - formatOff") {
    val code =
      """|// scalafmt: { maxColumn = 120 }
         |// format: off
         |object A
         |""".stripMargin
    val result = format(
      code,
      """fileHeader { text = "header", style = line }""",
    )
    assert(!result.contains("// header"), result)
  }

  test("directive before existing header - header replaced, directive preserved") {
    val code =
      """|// scalafmt: { maxColumn = 120 }
         |// Old Header
         |object A
         |""".stripMargin
    val result = format(
      code,
      """fileHeader { text = "New Header", style = line }""",
    )
    assert(result.contains("// New Header"), result)
    assert(!result.contains("Old Header"), result)
    assert(result.contains("scalafmt: { maxColumn = 120 }"), result)
    // Header should appear before directive
    val headerIdx = result.indexOf("// New Header")
    val directiveIdx = result.indexOf("scalafmt:")
    assert(headerIdx < directiveIdx, s"Header should precede directive:\n$result")
  }

  test("framed header with directive - idempotent") {
    val cfg =
      """|fileHeader {
         |  text = "Header"
         |  style = framed
         |  comment.width = 40
         |}
         |""".stripMargin
    val code =
      """|// scalafmt: { maxColumn = 120 }
         |object A
         |""".stripMargin
    val first = format(code, cfg)
    val second = format(first, cfg)
    assertEquals(second, first)
  }

  // --- Integration (full round-trip) ---

  test("license header inserted and code formatted") {
    val code = "object   A  {  val x=1  }\n"
    val result = format(
      code,
      """|fileHeader {
         |  license = MIT
         |  copyrightHolder = "Org"
         |  year = 2025
         |  style = line
         |}
         |""".stripMargin,
    )
    assert(result.contains("SPDX-License-Identifier: MIT"), result)
    assert(result.contains("object A"), result)
    // Code should be formatted (spaces cleaned up)
    assert(!result.contains("object   A"), result)
  }

  test("already correct header - idempotent") {
    val cfg =
      """|fileHeader {
         |  text = "My Header"
         |  style = line
         |}
         |""".stripMargin
    val code = "object A\n"
    val first = format(code, cfg)
    val second = format(first, cfg)
    assertEquals(second, first)
  }

  test("wrong header replaced") {
    val code =
      """|// Wrong Header
         |
         |object A
         |""".stripMargin
    val cfg =
      """|fileHeader {
         |  text = "Right Header"
         |  style = line
         |}
         |""".stripMargin
    val result = format(code, cfg)
    assert(result.contains("// Right Header"), result)
    assert(!result.contains("Wrong"), result)
  }

  test("range formatting - header NOT inserted") {
    val code = "object A { val x = 1 }\n"
    val cfg = ScalafmtConfig.fromHoconString(
      """fileHeader { text = "header", style = line }""",
    ).get
    val result = Scalafmt
      .format(code, cfg, range = Set(Range(0, 0))).get
    assert(!result.contains("header"), result)
  }

  test("header + lineEndings = windows - header gets CRLF") {
    val code = "object A\n"
    val cfg =
      """|fileHeader {
         |  text = "My Header"
         |  style = line
         |}
         |lineEndings = windows
         |""".stripMargin
    val result = format(code, cfg)
    assert(result.contains("\r\n"), result)
  }

  test("framed header with comments.wrap = no - preserved through formatter") {
    val cfg =
      """|fileHeader {
         |  text = "Header"
         |  style = framed
         |  comment.width = 40
         |}
         |comments.wrap = no
         |""".stripMargin
    val code = "object A\n"
    val first = format(code, cfg)
    val second = format(first, cfg)
    assertEquals(second, first)
  }

  // --- Missing coverage from audit ---

  test("licenseStyle = detailed produces full license text") {
    val cfg =
      """|fileHeader {
         |  license = MIT
         |  licenseStyle = detailed
         |  copyrightHolder = "Org"
         |  year = 2025
         |}
         |""".stripMargin
    val result = format("object A\n", cfg)
    assert(result.contains("Copyright 2025 Org"), result)
    assert(result.contains("Permission is hereby granted"), result)
    assert(result.contains("WITHOUT WARRANTY"), result)
    assert(!result.contains("SPDX-License-Identifier"), result)
  }

  test("blankLineAfter = false removes blank line between header and code") {
    val cfg =
      """|fileHeader {
         |  text = "My Header"
         |  style = line
         |  blankLineAfter = false
         |}
         |""".stripMargin
    val result = format("package com.example\n\nobject Main\n", cfg)
    assert(result.startsWith("// My Header\npackage"), result)
  }

  test("raw header starting with /** is preserved through formatter") {
    val cfg =
      """|fileHeader {
         |  raw = "/** Custom verbatim header */"
         |}
         |""".stripMargin
    val first = format("object A\n", cfg)
    assert(first.contains("/** Custom verbatim header */"), first)
    val second = format(first, cfg)
    assertEquals(second, first)
  }

  test("empty file with active header produces header + newline") {
    val result = format("\n", """fileHeader { text = "Header", style = line }""")
    assert(result.contains("// Header"), result)
  }
}
