package org.scalafmt.util

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import munit.{Assertions, FunSuite}
import org.scalactic.source.Position
import org.scalafmt.util.DeleteTree.deleteTree

import scala.util._

class GitOpsTest extends FunSuite {

  import GitOpsTest._

  val root = AbsoluteFile.userDir
  val dirName = "gitTestDir"

  // DESNOTE(2017-08-16, pjrt): Create a temporary git directory for each
  // test.
  private implicit var gitOps: GitOpsImpl = _
  private var path: Path = _

  override def beforeEach(context: BeforeEach): Unit = {
    path = Files.createTempDirectory(dirName)
    val absFile = AbsoluteFile.fromPath(path.toString).get
    gitOps = new GitOpsImpl(absFile)
    init(gitOps)
    // initial commit is needed
    val initF = touch("initialfile")
    add(initF)(gitOps)
    commit(gitOps)
  }

  override def afterEach(context: AfterEach): Unit = deleteTree(path)

  def touch(
      name: String = Random.alphanumeric.take(10).mkString,
      dir: Option[AbsoluteFile] = None
  ): AbsoluteFile = {
    val f =
      File.createTempFile(name, ".ext", dir.orElse(gitOps.rootDir).get.jfile)
    AbsoluteFile.fromPath(f.toString).get
  }

  def symbolicLinkTo(
      file: AbsoluteFile,
      name: String = Random.alphanumeric.take(10).mkString,
      dir: Option[AbsoluteFile] = None
  ): AbsoluteFile = {
    val linkFile =
      File.createTempFile(name, ".ext", dir.orElse(gitOps.rootDir).get.jfile)
    linkFile.delete()
    val link = AbsoluteFile.fromPath(linkFile.toString).get
    Files.createSymbolicLink(linkFile.toPath, file.jfile.toPath)
    link
  }

  def mv(f: AbsoluteFile, dir: Option[AbsoluteFile] = None): AbsoluteFile = {
    val destDir = Files.createTempDirectory(
      dir.orElse(gitOps.rootDir).get.jfile.toPath,
      "dir_"
    )
    val dest = Files.move(
      f.jfile.toPath,
      destDir,
      java.nio.file.StandardCopyOption.REPLACE_EXISTING
    )
    rm(f)
    AbsoluteFile.fromPath(dest.toString).get
  }

  def modify(f: AbsoluteFile): Unit = {
    val text = Random.alphanumeric.take(10).mkString
    Files.write(Paths.get(f.path), text.getBytes(StandardCharsets.UTF_8))
  }

  def ls(implicit ops: GitOpsImpl) =
    // DESNOTE(2017-08-17, pjrt): Filter out the initial file since it will
    // just annoy us in the tests below
    ops
      .lsTree(ops.workingDirectory)
      .filterNot(_.jfile.getName().contains("initialfile"))

  def mkDir(
      dirName: String = Random.alphanumeric.take(10).mkString
  ): AbsoluteFile = {
    val file = new File(gitOps.rootDir.get.jfile, dirName)
    file.mkdir()
    AbsoluteFile.fromFile(file, gitOps.workingDirectory)
  }

  test("lsTree should not return files not added to the index") {
    touch()
    assert(ls.isEmpty)
  }

  test("#1010: lsTree should return staged files") {
    val f = touch()
    add(f)
    assert(ls == Seq(f))
  }

  test("lsTree should return committed files") {
    val f = touch()
    add(f)
    commit
    assert(ls == Seq(f))
  }

  test("lsTree should exclude symbolic links") {
    val f = touch()
    add(f)
    val g = symbolicLinkTo(f)
    add(g)
    commit
    assert(ls == Seq(f))
  }

  test("lsTree should not return committed files that have been deleted") {
    val f = touch()
    add(f)
    commit
    rm(f)
    assert(ls.isEmpty)
  }

  test(
    "lsTree should return files properly when the working directory is under the git root directory"
  ) {
    val f1 = touch()
    add(f1)

    val innerDir = mkDir()
    val f2 = touch(dir = Some(innerDir))
    add(f2)

    val innerGitOps = new GitOpsImpl(innerDir)
    assert(ls(innerGitOps) == Seq(f2))
  }

  test("lsTree should return committed files that have been modified") {
    val f = touch()
    add(f)
    commit
    modify(f)
    assert(ls == Seq(f))
  }

  def diff(br: String = "HEAD")(implicit ops: GitOpsImpl): Seq[AbsoluteFile] =
    ops.diff(br)

  def status(implicit ops: GitOpsImpl): Seq[AbsoluteFile] =
    ops.status

  // diff
  test("diff should return modified committed files") {
    val f = touch()
    add(f)
    commit
    modify(f)
    assert(diff() == Seq(f))
  }

  test("#1000: diff should not return git deleted files") {
    val f = touch()
    add(f)
    commit
    rm(f)
    assert(diff().isEmpty)
  }

  test("#1000: diff should not return fs deleted files") {
    val f = touch()
    add(f)
    commit
    rmfs(f)
    assert(diff().isEmpty)
  }

  test("diff should return added files against HEAD") {
    val f1 = touch()
    val f2 = touch()
    add(f1)
    add(f2)
    assert(diff().toSet == Set(f1, f2))
  }

  test("diff should return added files against a different branch") {
    val f = touch()
    add(f)
    commit
    checkoutBr("other")
    val f1 = touch()
    val f2 = touch()
    add(f1)
    add(f2)
    commit
    assert(diff("master").toSet == Set(f1, f2))
  }

  test(
    "diff should return added files that are then modified against a different branch"
  ) {
    val f = touch()
    add(f)
    commit
    checkoutBr("other")
    val f1 = touch()
    val f2 = touch()
    add(f1)
    add(f2)
    modify(f1)
    assert(diff("master").toSet == Set(f1, f2))
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
    assert(diff("master") == Seq(f2))
  }

  test("status should return only modified files") {
    val f = touch()
    add(f)
    commit
    val f1 = touch()
    assert(status == Seq(f1))
  }

  test("status should return moved") {
    val f = touch()
    add(f)
    commit
    val f1 = mv(f)
    add(f1)
    assert(status == Seq(f1))
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
    assert(status == Seq(f1))
  }

  test("status should return files with spaces in the path") {
    val dir = mkDir("dir 1")
    val f = touch(dir = Option(dir))
    add(f)
    assert(status == Seq(f))
  }

}

private object GitOpsTest {

  // Filesystem commands
  def rmfs(file: AbsoluteFile): Unit =
    file.jfile.delete

  // Git commands
  def git(str: String*)(implicit ops: GitOpsImpl, pos: Position): Seq[String] =
    ops.exec("git" +: str) match {
      case Failure(f) => Assertions.fail(s"Failed git command. Got: $f")
      case Success(s) => s
    }

  def init(implicit ops: GitOpsImpl): Unit =
    git("init")

  def add(file: AbsoluteFile)(implicit ops: GitOpsImpl): Unit =
    git("add", file.toString())

  def rm(file: AbsoluteFile)(implicit ops: GitOpsImpl): Unit =
    git("rm", file.toString())

  def commit(implicit ops: GitOpsImpl): Unit =
    git("commit", "-m", "'some-message'")

  def checkout(br: String)(implicit ops: GitOpsImpl): Unit =
    git("checkout", "$br")

  def checkoutBr(newBr: String)(implicit ops: GitOpsImpl): Unit =
    git("checkout", "-b", "$newBr")
}
