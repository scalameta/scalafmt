package org.scalafmt.sysops

class FileOpsJVMTest extends munit.FunSuite {
  test("readFile with URL") {
    val url = getClass.getResource("/readme.md")
    val contents = FileOps.readFile(url.toString)
    val expectedFirstLine = "# scalafmt tests\n"
    val firstLine = contents.substring(0, expectedFirstLine.length)
    assertEquals(firstLine, expectedFirstLine)

    assertEquals(FileOps.readFile(url), contents)
    assertEquals(FileOps.readAsURL(url), contents)
  }
}
