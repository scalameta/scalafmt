package org.scalafmt.sysops

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.util.Random

class FileOpsTest extends munit.FunSuite {

  import FileOpsTest._

  private val F = FunFixture[Path](
    _ => PlatformFileOps.mkdtemp("FileOpsTestDir"),
    path =>
      try DeleteTree(path)
      catch {
        case e: Throwable =>
          println(s"Unable to delete test files: $path")
          e.printStackTrace()
      },
  )

  F.test("listFiles") { path =>
    assertEquals(FileOps.listFiles(path), Nil)

    val subfile = subpath(path)
    PlatformFileOps.writeFile(subfile, "file")(StandardCharsets.UTF_8)
    assertEquals(FileOps.listFiles(path), Seq(subfile))
    assertEquals(FileOps.listFiles(subfile), Seq(subfile))

    val subdir = subpath(path)
    PlatformFileOps.mkdir(subdir)
    assertEquals(FileOps.listFiles(path), Seq(subfile))
    assertEquals(FileOps.listFiles(subdir), Nil)

    val subsubfile = subpath(subdir)
    PlatformFileOps.writeFile(subsubfile, "file")(StandardCharsets.UTF_8)
    assertEquals(FileOps.listFiles(path).toSet, Set(subfile, subsubfile))
    assertEquals(FileOps.listFiles(subdir), Seq(subsubfile))
    assertEquals(FileOps.listFiles(subsubfile), Seq(subsubfile))
  }

}

object FileOpsTest {

  private def subpath(path: Path): Path = path
    .resolve(Random.alphanumeric.take(10).mkString)

}
