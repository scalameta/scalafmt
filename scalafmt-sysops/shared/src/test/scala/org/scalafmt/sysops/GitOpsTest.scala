package org.scalafmt.sysops

import java.nio.charset.StandardCharsets

import scala.util._

import munit.{Assertions, FunSuite, Location}

class GitOpsTest extends FunSuite {

  import GitOpsTest._

  private val F = FunFixture[Session](_ => new Session(), s => s.teardown())

  F.test("lsTree should not return files not added to the index") { s =>
    import s._
    touch()
    assertEquals(ls, Seq.empty)
  }

  F.test("#1010: lsTree should return staged files") { s =>
    import s._
    val f = touch()
    add(f)
    val q = ls
    assertEquals(q.toSet, Set(f), q.mkString + " != " + f.toString())
  }

  F.test("lsTree should return committed files") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    assertEquals(ls.toSet, Set(f))
  }

  F.test("lsTree should exclude symbolic links") { s =>
    import s._
    val f = touch()
    add(f)

    val g = getTempFile(None)
    PlatformFileOps.symlink(g.path, f.path)

    add(g)
    commit
    assertEquals(ls.toSet, Set(f))
  }

  F.test("lsTree skips committed files that have been deleted") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    rm(f)
    assertEquals(ls, Seq.empty)
  }

  F.test("lsTree shows files properly when CWD is under git root") { s =>
    import s._
    val f1 = touch()
    add(f1)

    val innerDir = mkDir()
    val f2 = touch(innerDir)
    add(f2)

    val innerGitOps = new GitOpsImpl(innerDir)
    assertEquals(lsTree(innerGitOps).toSet, Set(f2))
  }

  F.test("lsTree shows committed files that have been modified") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    modify(f)
    assertEquals(ls.toSet, Set(f))
  }

  // diff
  F.test("diff shows modified committed files") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    modify(f)
    assertEquals(diff().toSet, Set(f))
  }

  F.test("diff shows modified files from specific subdirs") { s =>
    import s._
    val d1 = mkDir()
    val d2 = mkDir()
    val d3 = mkDir()

    val f0 = touch()
    val f1 = touch(dir = d1)
    val f2 = touch(dir = d2)
    val f3 = touch(dir = d3)

    add(f0, f1, f2, f3)
    assertEquals(diff(d1, d2).toSet, Set(f1, f2))
    assertEquals(diff().toSet, Set(f0, f1, f2, f3))
    assertEquals(diff(path).toSet, Set(f0, f1, f2, f3))
  }

  F.test("#1000: diff should not return git deleted files") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    rm(f)
    assertEquals(diff(), Seq.empty)
  }

  F.test("#1000: diff should not return fs deleted files") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    rmfs(f)
    assertEquals(diff(), Seq.empty)
  }

  F.test("diff should return added files against HEAD") { s =>
    import s._
    val dir = mkDir("dir 1")
    val f1 = touch()
    val f2 = touch(dir = dir)
    add(f1)
    add(f2)
    assertEquals(diff().toSet, Set(f1, f2))
    assertEquals(diff(cwd = dir).toSet, Set(f2))
  }

  F.test("diff shows added files against a different branch") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    checkoutBr("other")
    val dir = mkDir("dir 1")
    val f1 = touch()
    val f2 = touch(dir = dir)
    add(f1)
    add(f2)
    commit
    assertEquals(diff(defaultBranch).toSet, Set(f1, f2))
    assertEquals(diff(defaultBranch, dir).toSet, Set(f2))
  }

  F.test("diff shows added-then-modified files against another branch") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    checkoutBr("other")
    val dir = mkDir("dir 1")
    val f1 = touch()
    val f2 = touch(dir = dir)
    add(f1)
    add(f2)
    modify(f1)
    assertEquals(diff(defaultBranch).toSet, Set(f1, f2))
    assertEquals(diff(defaultBranch, dir).toSet, Set(f2))
  }

  F.test("diff skips removed files against a different branch") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    checkoutBr("other")
    val f1 = touch()
    val f2 = touch()
    add(f1)
    add(f2)
    commit
    rm(f1)
    assertEquals(diff(defaultBranch).toSet, Set(f2))
  }

  F.test("status should return only modified files") { s =>
    import s._
    val f = touch()
    add(f)
    commit
    val f1 = touch()
    assertEquals(status().toSet, Set(f1))
  }

  F.test("status should return modified files from specific subdirs") { s =>
    import s._
    val d1 = mkDir()
    val d2 = mkDir()
    val d3 = mkDir()

    val f0 = touch()
    val f1 = touch(dir = d1)
    val f2 = touch(dir = d2)
    val f3 = touch(dir = d3)

    add(f0, f1, f2, f3)
    assertEquals(status(d1, d2).toSet, Set(f1, f2))
    assertEquals(status().toSet, Set(f0, f1, f2, f3))
    assertEquals(status(path).toSet, Set(f0, f1, f2, f3))
  }

  F.test("status should return moved") { s =>
    import s._
    val f = touch()
    add(f)
    commit

    val newf = FileOps.getPath(s"$f.moved")
    PlatformFileOps.move(f.path, newf)
    rm(f)
    val f1 = AbsoluteFile(newf)

    add(f1)
    assertEquals(status().toSet, Set(f1))
  }

  F.test("status should not return deleted files") { s =>
    import s._
    val f = touch()
    modify(f)
    add(f)
    commit
    val f1 = touch()
    modify(f1)
    add(f1)
    rm(f)
    assertEquals(status().toSet, Set(f1))
  }

  F.test("status should return files with spaces in the path") { s =>
    import s._
    val dir = mkDir("dir 1")
    val f = touch(dir = dir)
    add(f)
    assertEquals(status().toSet, Set(f))
  }

  F.test("lsTree should return files from specific subdirs") { s =>
    import s._
    val d1 = mkDir()
    val d2 = mkDir()
    val d3 = mkDir()

    val f1 = touch(dir = d1)
    val f2 = touch(dir = d2)
    val f3 = touch(dir = d3)

    add(f1, f2, f3)
    assertEquals(ops.lsTree(d1, d2).toSet, Set(f1, f2))
    assertEquals(ops.lsTree().toSet, Set(initFile, f1, f2, f3))
    assertEquals(ops.lsTree(path).toSet, Set(initFile, f1, f2, f3))
  }

}

private object GitOpsTest {

  private final val defaultBranch = "main"

  // Filesystem commands
  def rmfs(file: AbsoluteFile): Unit = file.delete()

  // Git commands
  def git(args: Any*)(implicit ops: GitOpsImpl, loc: Location): Seq[String] =
    ops.tryExec(args: _*)
      .fold(ex => Assertions.fail(s"Failed git command. Got: $ex"), identity)

  def init(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("init", "-b", defaultBranch)

  def add(
      file: AbsoluteFile*,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit = git("add", file)

  def rm(
      file: AbsoluteFile*,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit = git("rm", file)

  def commit(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("commit", "-m", "'some-message'")

  def checkout(
      br: String,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit = git("checkout", br)

  def checkoutBr(
      newBr: String,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("checkout", "-b", newBr)

  def getTempFile(
      dir: Option[AbsoluteFile],
      name: String = Random.alphanumeric.take(10).mkString,
  )(implicit ops: GitOpsImpl): AbsoluteFile = dir.orElse(ops.rootDir).get
    .join(s"$name.ext")

  def diff(br: String, cwd: AbsoluteFile*)(implicit
      ops: GitOpsImpl,
  ): Seq[AbsoluteFile] = ops.diff(br, cwd: _*)

  def diff(cwd: AbsoluteFile*)(implicit ops: GitOpsImpl): Seq[AbsoluteFile] =
    diff("HEAD", cwd: _*)

  def status(cwd: AbsoluteFile*)(implicit ops: GitOpsImpl): Seq[AbsoluteFile] =
    ops.status(cwd: _*)

  def modify(f: AbsoluteFile): Unit = {
    val text = Random.alphanumeric.take(10).mkString
    f.writeFile(text)(StandardCharsets.UTF_8)
  }

  def mkDir(
      dirName: String = Random.alphanumeric.take(10).mkString,
  )(implicit ops: GitOpsImpl): AbsoluteFile = {
    val file = ops.rootDir.getOrElse(ops.workingDirectory) / dirName
    file.mkdir()
    file
  }

  def touch(dir: AbsoluteFile)(implicit ops: GitOpsImpl): AbsoluteFile =
    touch(dir = Some(dir))

  def touch(
      name: String = Random.alphanumeric.take(10).mkString,
      dir: Option[AbsoluteFile] = None,
  )(implicit ops: GitOpsImpl): AbsoluteFile = {
    val file = getTempFile(dir, name)
    file.writeFile("")
    file
  }

  def lsTree(ops: GitOpsImpl): Seq[AbsoluteFile] = ops
    .lsTree(ops.workingDirectory)

  class Session {

    // DESNOTE(2017-08-16, pjrt): Create a temporary git directory for each
    // test.
    val path: AbsoluteFile = AbsoluteFile(PlatformFileOps.mkdtemp("gitTestDir"))
    implicit val ops: GitOpsImpl = new GitOpsImpl(path)
    init
    // initial commit is needed
    val initFile: AbsoluteFile = touch("initialfile")
    add(initFile)
    commit

    def teardown(): Unit =
      try DeleteTree(path.path)
      catch {
        case e: Throwable =>
          println(s"Unable to delete test files: $path")
          e.printStackTrace()
      }

    def ls: Seq[AbsoluteFile] =
      // DESNOTE(2017-08-17, pjrt): Filter out the initial file since it will
      // just annoy us in the tests below
      lsTree(ops).filterNot(_ == initFile)

  }

}
