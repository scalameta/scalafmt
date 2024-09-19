package org.scalafmt.sysops

import org.scalafmt.util.DeleteTree.deleteTree

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.util.Random

class FileOpsTest extends munit.FunSuite {

  import FileOpsTest._

  private var path: Path = _

  override def beforeEach(context: BeforeEach): Unit =
    path = Files.createTempDirectory("FileOpsTestDir")

  override def afterEach(context: AfterEach): Unit =
    try deleteTree(path)
    catch {
      case e: Throwable =>
        println("Unable to delete test files")
        e.printStackTrace()
    }

  test("listFiles") {
    assertEquals(FileOps.listFiles(path), Nil)

    val subfile = subpath(path)
    Files.write(subfile, "file".getBytes(StandardCharsets.UTF_8))
    assertEquals(FileOps.listFiles(path), Seq(subfile))
    assertEquals(FileOps.listFiles(subfile), Seq(subfile))

    val subdir = subpath(path)
    Files.createDirectory(subdir)
    assertEquals(FileOps.listFiles(path), Seq(subfile))
    assertEquals(FileOps.listFiles(subdir), Nil)

    val subsubfile = subpath(subdir)
    Files.write(subsubfile, "file".getBytes(StandardCharsets.UTF_8))
    assertEquals(FileOps.listFiles(path).toSet, Set(subfile, subsubfile))
    assertEquals(FileOps.listFiles(subdir), Seq(subsubfile))
    assertEquals(FileOps.listFiles(subsubfile), Seq(subsubfile))
  }

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

object FileOpsTest {

  private def subpath(path: Path): Path = path
    .resolve(Random.alphanumeric.take(10).mkString)

}
