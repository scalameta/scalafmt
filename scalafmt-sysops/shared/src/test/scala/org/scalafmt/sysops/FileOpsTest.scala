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
    assertEquals(FileOps.walkFiles(FileOps.WalkVisitor.empty)(path), Nil)

    val subfile = subpath(path)
    PlatformFileOps.writeFile(subfile, "file")(StandardCharsets.UTF_8)
    assertEquals(FileOps.listFiles(path), Seq(subfile))
    assertEquals(FileOps.listFiles(subfile), Seq(subfile))
    assertEquals(FileOps.walkFiles(FileOps.WalkVisitor.empty)(path), Seq(subfile))
    assertEquals(
      FileOps.walkFiles(FileOps.WalkVisitor.empty)(subfile),
      Seq(subfile),
    )
    assertEquals(
      FileOps.walkFiles(new FileOps.WalkVisitor {
        override def onTree(dir: Path, fileStat: FileStat): FileOps.WalkVisit =
          if (dir != path) FileOps.WalkVisit.Good else FileOps.WalkVisit.Skip
      })(path),
      Nil,
    )

    val subdir = subpath(path)
    PlatformFileOps.mkdir(subdir)
    assertEquals(FileOps.listFiles(path), Seq(subfile))
    assertEquals(FileOps.listFiles(subdir), Nil)

    val subsubfile = subpath(subdir)
    PlatformFileOps.writeFile(subsubfile, "file")(StandardCharsets.UTF_8)
    assertEquals(FileOps.listFiles(path).toSet, Set(subfile, subsubfile))
    assertEquals(FileOps.listFiles(subdir), Seq(subsubfile))
    assertEquals(FileOps.listFiles(subsubfile), Seq(subsubfile))
    assertEquals(
      FileOps.walkFiles(new FileOps.WalkVisitor {
        override def onTree(dir: Path, fileStat: FileStat): FileOps.WalkVisit =
          if (dir.startsWith(subdir)) FileOps.WalkVisit.Skip
          else FileOps.WalkVisit.Good
      })(path),
      Seq(subfile),
    )
  }

}

object FileOpsTest {

  private def subpath(path: Path): Path = path
    .resolve(Random.alphanumeric.take(10).mkString)

}
