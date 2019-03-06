package org.scalafmt.util

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.util._

import org.scalatest._
import org.scalactic.source.Position

class GitOpsTest extends fixture.FunSuite {

  import Matchers._
  import GitOpsTest._

  val root = AbsoluteFile.userDir
  val dirName = "gitTestDir"

  override type FixtureParam = GitOpsImpl

  // DESNOTE(2017-08-16, pjrt): Create a temporary git directory for each
  // test.
  override def withFixture(test: OneArgTest) = {
    val f = Files.createTempDirectory(dirName)
    val absFile = AbsoluteFile.fromPath(f.toString).get
    val ops = new GitOpsImpl(absFile)
    init(ops)
    // initial commit is needed
    val initF = touch("initialfile")(ops)
    add(initF)(ops)
    commit(ops)
    val t = try test.toNoArgTest(ops)
    finally f.toFile.delete
    withFixture(t)
  }

  def touch(
      name: String = Random.alphanumeric.take(10).mkString,
      dir: Option[AbsoluteFile] = None
  )(implicit ops: GitOpsImpl): AbsoluteFile = {
    val f = File.createTempFile(name, ".ext", dir.orElse(ops.rootDir).get.jfile)
    AbsoluteFile.fromPath(f.toString).get
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
  )(implicit ops: GitOpsImpl): AbsoluteFile = {
    val file = new File(ops.rootDir.get.jfile, dirName)
    file.mkdir()
    AbsoluteFile.fromFile(file, ops.workingDirectory)
  }

  test("lsTree should not return files not added to the index") {
    implicit ops =>
      touch()
      ls shouldBe empty
  }

  test("#1010: lsTree should return staged files") { implicit ops =>
    val f = touch()
    add(f)
    ls should contain only (f)
  }

  test("lsTree should return committed files") { implicit ops =>
    val f = touch()
    add(f)
    commit
    ls should contain only (f)
  }

  test("lsTree should not return committed files that have been deleted") {
    implicit ops =>
      val f = touch()
      add(f)
      commit
      rm(f)
      ls shouldBe empty
  }

  test(
    "lsTree should return files properly when the working directory is under the git root directory"
  ) { implicit ops =>
    val f1 = touch()
    add(f1)

    val innerDir = mkDir()
    val f2 = touch(dir = Some(innerDir))
    add(f2)

    val innerGitOps = new GitOpsImpl(innerDir)
    ls(innerGitOps) should contain only f2
  }

  test("lsTree should return committed files that have been modified") {
    implicit ops =>
      val f = touch()
      add(f)
      commit
      modify(f)
      ls should contain only (f)
  }

  def diff(br: String = "HEAD")(implicit ops: GitOpsImpl): Seq[AbsoluteFile] =
    ops.diff(br)

  // diff
  test("diff should return modified committed files") { implicit o =>
    val f = touch()
    add(f)
    commit
    modify(f)
    diff() should contain only (f)
  }

  test("#1000: diff should not return git deleted files") { implicit o =>
    val f = touch()
    add(f)
    commit
    rm(f)
    diff() shouldBe empty
  }

  test("#1000: diff should not return fs deleted files") { implicit o =>
    val f = touch()
    add(f)
    commit
    rmfs(f)
    diff() shouldBe empty
  }

  test("diff should return added files against HEAD") { implicit o =>
    val f1 = touch()
    val f2 = touch()
    add(f1)
    add(f2)
    diff() should contain only (f1, f2)
  }

  test("diff should return added files against a different branch") {
    implicit o =>
      val f = touch()
      add(f)
      commit
      checkoutBr("other")
      val f1 = touch()
      val f2 = touch()
      add(f1)
      add(f2)
      commit
      diff("master") should contain only (f1, f2)
  }

  test(
    "diff should return added files that are then modified against a different branch"
  ) { implicit o =>
    val f = touch()
    add(f)
    commit
    checkoutBr("other")
    val f1 = touch()
    val f2 = touch()
    add(f1)
    add(f2)
    modify(f1)
    diff("master") should contain only (f1, f2)
  }

  test("diff should not return removed files against a different branch") {
    implicit o =>
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
      diff("master") should contain only (f2)
  }

}

private object GitOpsTest {

  import OptionValues._
  import Matchers._

  // Filesystem commands
  def rmfs(file: AbsoluteFile): Unit =
    file.jfile.delete

  // Git commands
  def git(str: String)(implicit ops: GitOpsImpl, pos: Position): Seq[String] =
    ops.exec("git" +: str.split(' ').toSeq) match {
      case Failure(f) => fail(s"Failed git command. Got: $f")
      case Success(s) => s
    }

  def init(implicit ops: GitOpsImpl): Unit =
    git("init")

  def add(file: AbsoluteFile)(implicit ops: GitOpsImpl): Unit =
    git(s"add $file")

  def rm(file: AbsoluteFile)(implicit ops: GitOpsImpl): Unit =
    git(s"rm $file")

  def commit(implicit ops: GitOpsImpl): Unit =
    git(s"commit -m 'some-message'")

  def checkout(br: String)(implicit ops: GitOpsImpl): Unit =
    git(s"checkout $br")

  def checkoutBr(newBr: String)(implicit ops: GitOpsImpl): Unit =
    git(s"checkout -b $newBr")
}
