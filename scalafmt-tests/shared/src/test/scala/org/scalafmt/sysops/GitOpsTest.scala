package org.scalafmt.sysops

import org.scalafmt.util.DeleteTree.deleteTree

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.util._

import munit.Assertions
import munit.FunSuite
import munit.Location

class GitOpsTest extends FunSuite {

  import GitOpsTest._

  val root = AbsoluteFile.userDir
  val dirName = "gitTestDir"

  // DESNOTE(2017-08-16, pjrt): Create a temporary git directory for each
  // test.
  private implicit var ops: GitOpsImpl = _
  private var path: AbsoluteFile = _
  private var initFile: AbsoluteFile = _

  override def beforeEach(context: BeforeEach): Unit = {
    path = AbsoluteFile(Files.createTempDirectory(dirName))
    ops = new GitOpsImpl(path)
    init
    // initial commit is needed
    initFile = touch("initialfile")
    add(initFile)
    commit
  }

  override def afterEach(context: AfterEach): Unit =
    try deleteTree(path.path)
    catch {
      case e: Throwable =>
        println("Unable to delete test files")
        e.printStackTrace()
    }

  private def touch(dir: AbsoluteFile): AbsoluteFile = touch(dir = Some(dir))

  private def touch(
      name: String = Random.alphanumeric.take(10).mkString,
      dir: Option[AbsoluteFile] = None,
  ): AbsoluteFile = {
    val d = dir.orElse(ops.rootDir).get.jfile
    val f = File.createTempFile(name, ".ext", d)
    f.deleteOnExit()
    AbsoluteFile(f)
  }

  def symbolicLinkTo(
      file: AbsoluteFile,
      name: String = Random.alphanumeric.take(10).mkString,
      dir: Option[AbsoluteFile] = None,
  ): AbsoluteFile = {
    val linkFile = File
      .createTempFile(name, ".ext", dir.orElse(ops.rootDir).get.jfile)
    linkFile.delete()
    val link = AbsoluteFile(linkFile)
    Files.createSymbolicLink(link.path, file.path)
    link
  }

  def mv(f: AbsoluteFile, dir: Option[AbsoluteFile] = None): AbsoluteFile = {
    val destDir = Files
      .createTempDirectory(dir.orElse(ops.rootDir).get.path, "dir_")
    val dest = Files
      .move(f.path, destDir, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    rm(f)
    AbsoluteFile(dest)
  }

  def modify(f: AbsoluteFile): Unit = {
    val text = Random.alphanumeric.take(10).mkString
    f.writeFile(text)(StandardCharsets.UTF_8)
  }

  def ls(implicit ops: GitOpsImpl) =
    // DESNOTE(2017-08-17, pjrt): Filter out the initial file since it will
    // just annoy us in the tests below
    ops.lsTree(ops.workingDirectory).filterNot(_ == initFile)

  def mkDir(
      dirName: String = Random.alphanumeric.take(10).mkString,
  ): AbsoluteFile = {
    val file = ops.rootDir.getOrElse(ops.workingDirectory) / dirName
    file.mkdir()
    file
  }

  test("lsTree should not return files not added to the index") {
    touch()
    assertEquals(ls, Seq.empty)
  }

  test("#1010: lsTree should return staged files") {
    val f = touch()
    add(f)
    val q = ls
    assertEquals(q.toSet, Set(f), q.mkString + " != " + f.toString())
  }

  test("lsTree should return committed files") {
    val f = touch()
    add(f)
    commit
    assertEquals(ls.toSet, Set(f))
  }

  test("lsTree should exclude symbolic links") {
    val f = touch()
    add(f)
    val g = symbolicLinkTo(f)
    add(g)
    commit
    assertEquals(ls.toSet, Set(f))
  }

  test("lsTree should not return committed files that have been deleted") {
    val f = touch()
    add(f)
    commit
    rm(f)
    assertEquals(ls, Seq.empty)
  }

  test("lsTree should return files properly when the working directory is under the git root directory") {
    val f1 = touch()
    add(f1)

    val innerDir = mkDir()
    val f2 = touch(innerDir)
    add(f2)

    val innerGitOps = new GitOpsImpl(innerDir)
    assertEquals(ls(innerGitOps).toSet, Set(f2))
  }

  test("lsTree should return committed files that have been modified") {
    val f = touch()
    add(f)
    commit
    modify(f)
    assertEquals(ls.toSet, Set(f))
  }

  def diff(br: String, cwd: AbsoluteFile*)(implicit
      ops: GitOpsImpl,
  ): Seq[AbsoluteFile] = ops.diff(br, cwd: _*)

  def diff(cwd: AbsoluteFile*)(implicit ops: GitOpsImpl): Seq[AbsoluteFile] =
    diff("HEAD", cwd: _*)

  def status(cwd: AbsoluteFile*)(implicit ops: GitOpsImpl): Seq[AbsoluteFile] =
    ops.status(cwd: _*)

  // diff
  test("diff should return modified committed files") {
    val f = touch()
    add(f)
    commit
    modify(f)
    assertEquals(diff().toSet, Set(f))
  }

  test("diff should return modified files from specific subdirs") {
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

  test("#1000: diff should not return git deleted files") {
    val f = touch()
    add(f)
    commit
    rm(f)
    assertEquals(diff(), Seq.empty)
  }

  test("#1000: diff should not return fs deleted files") {
    val f = touch()
    add(f)
    commit
    rmfs(f)
    assertEquals(diff(), Seq.empty)
  }

  test("diff should return added files against HEAD") {
    val dir = mkDir("dir 1")
    val f1 = touch()
    val f2 = touch(dir = dir)
    add(f1)
    add(f2)
    assertEquals(diff().toSet, Set(f1, f2))
    assertEquals(diff(cwd = dir).toSet, Set(f2))
  }

  test("diff should return added files against a different branch") {
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

  test("diff should return added files that are then modified against a different branch") {
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

  test("diff should not return removed files against a different branch") {
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

  test("status should return only modified files") {
    val f = touch()
    add(f)
    commit
    val f1 = touch()
    assertEquals(status().toSet, Set(f1))
  }

  test("status should return modified files from specific subdirs") {
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

  test("status should return moved") {
    val f = touch()
    add(f)
    commit
    val f1 = mv(f)
    add(f1)
    assertEquals(status().toSet, Set(f1))
  }

  test("status should not return deleted files") {
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

  test("status should return files with spaces in the path") {
    val dir = mkDir("dir 1")
    val f = touch(dir = dir)
    add(f)
    assertEquals(status().toSet, Set(f))
  }

  test("lsTree should return files from specific subdirs") {
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
  def git(cmd: String, args: String*)(implicit
      ops: GitOpsImpl,
      loc: Location,
  ): Seq[String] = Try(ops.exec("git" +: cmd +: args)) match {
    case Failure(f) => Assertions.fail(s"Failed git command. Got: $f")
    case Success(s) => s
  }

  def init(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("init", "-b", defaultBranch)

  def add(
      file: AbsoluteFile*,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("add", file.map(_.toString()): _*)

  def rm(
      file: AbsoluteFile*,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("rm", file.map(_.toString()): _*)

  def commit(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("commit", "-m", "'some-message'")

  def checkout(
      br: String,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit = git("checkout", br)

  def checkoutBr(
      newBr: String,
  )(implicit ops: GitOpsImpl, loc: munit.Location): Unit =
    git("checkout", "-b", newBr)
}
