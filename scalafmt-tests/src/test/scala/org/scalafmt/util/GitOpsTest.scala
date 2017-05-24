package org.scalafmt.util

import java.io.File

import org.scalatest.FunSuite

class GitOpsTest extends FunSuite {
  test("lsTree") {
    val files = GitOps().lsTree(AbsoluteFile.userDir)
    assert(files.exists(x => x.path.endsWith(s".gitignore")))
  }
}
