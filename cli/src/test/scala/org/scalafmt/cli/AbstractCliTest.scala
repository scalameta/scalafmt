package org.scalafmt.cli

import java.io.File

import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

abstract class AbstractCliTest extends FunSuite with DiffAssertions {
  def getMockOptions(baseDir: AbsoluteFile): CliOptions =
    getMockOptions(baseDir, baseDir)

  def getMockOptions(baseDir: AbsoluteFile,
                     workingDir: AbsoluteFile): CliOptions = {
    CliOptions.default.copy(
      gitOpsConstructor = x => new FakeGitOps(baseDir),
      common = CliOptions.default.common.copy(
        workingDirectory = workingDir
      )
    )
  }

  val baseCliOptions: CliOptions = getMockOptions(
    AbsoluteFile
      .fromPath(File.createTempFile("base", "dir").getAbsolutePath)
      .get)

  def getConfig(args: Array[String]): CliOptions = {
    Cli.getConfig(args, baseCliOptions).get
  }

}
