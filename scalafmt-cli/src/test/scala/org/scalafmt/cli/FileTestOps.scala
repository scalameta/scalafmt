package org.scalafmt.cli

import java.io.{File, PrintStream}
import java.nio.file.Files

import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.FileOps

object FileTestOps {

  /**
    * The inverse of [[dir2string]]. Given a string representation creates the
    * necessary files/directories with respective file contents.
    */
  def string2dir(layout: String): AbsoluteFile = {
    val root = Files.createTempDirectory("root").toFile
    layout.split("(?=\n/)").foreach { row =>
      val path :: contents :: Nil =
        row.stripPrefix("\n").split("\n", 2).toList
      val file = new File(root, path)
      file.getParentFile.mkdirs()
      FileOps.writeFile(file, contents)
    }
    AbsoluteFile.fromPath(root.getAbsolutePath).get
  }

  /** Gives a string representation of a directory. For example
    *
    * /build.sbt
    * val x = project
    * /src/main/scala/Main.scala
    * object A { def main = Unit }
    * /target/scala-2.11/foo.class
    * ^!*@#@!*#&@*!&#^
    */
  def dir2string(file: AbsoluteFile): String = {
    FileOps
      .listFiles(file.jfile)
      .sorted
      .map { path =>
        val contents = FileOps.readFile(path)
        s"""|${path.stripPrefix(file.jfile.getPath)}
            |$contents""".stripMargin
      }
      .mkString("\n")
      .replace(File.separator, "/") // ensure original separators
  }

  def getMockOptions(baseDir: AbsoluteFile): CliOptions =
    getMockOptions(baseDir, baseDir)

  def getMockOptions(
      baseDir: AbsoluteFile,
      workingDir: AbsoluteFile,
      out: PrintStream = System.out
  ): CliOptions = {
    CliOptions.default.copy(
      gitOpsConstructor = _ => new FakeGitOps(baseDir),
      common = CliOptions.default.common.copy(
        workingDirectory = workingDir,
        out = out,
        err = out
      )
    )
  }

  val baseCliOptions: CliOptions = getMockOptions(
    AbsoluteFile
      .fromPath(Files.createTempDirectory("base-dir").toString)
      .get
  )
}
