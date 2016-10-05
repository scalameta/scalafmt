package org.scalafmt.util

import java.io.File

import org.scalatest.FunSuite

class GitOpsTest extends FunSuite {
  test("lsTree") {
    val files = GitOps.lsTree
    assert(files.exists(x => new File(x).getPath == ".gitignore"))
  }
}
