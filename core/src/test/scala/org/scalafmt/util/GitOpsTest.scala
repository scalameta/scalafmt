package org.scalafmt.util

import org.scalatest.FunSuite

class GitOpsTest extends FunSuite {
  test("lsTree") {
    val files = GitOps.lsTree
    assert(files.exists(_.getPath == ".gitignore"))
  }
}
