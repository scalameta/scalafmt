package org.scalafmt.cli

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.RegexCompat
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.FileOps

import java.io.File
import java.io.PrintStream
import java.nio.file.Files

object FileTestOps {

  /** The inverse of [[dir2string]]. Given a string representation creates the
    * necessary files/directories with respective file contents.
    */
  def string2dir(layout: String): AbsoluteFile = {
    val root = AbsoluteFile(Files.createTempDirectory("root"))
    RegexCompat.splitByBeforeTextMatching(layout, "\n/").foreach { row =>
      val path :: contents :: Nil = row.stripPrefix("\n").split("\n", 2).toList
      val file = root / path.stripPrefix("/")
      file.parent.mkdirs()
      file.writeFile(contents)
    }
    root
  }

  /** Gives a string representation of a directory. For example
    * {{{
    * /build.sbt
    * val x = project
    * /src/main/scala/Main.scala
    * object A { def main = Unit }
    * /target/scala-2.11/foo.class
    * ^!*@#@!*#&@*!&#^
    * }}}
    */
  def dir2string(file: AbsoluteFile): String = {
    val rootPath = file.path
    val prefix = rootPath.toString
    FileOps.listFiles(rootPath).sortBy(_.toString).map(path =>
      s"""|${path.toString.stripPrefix(prefix)}
          |${FileOps.readFile(path)}""".stripMargin,
    ).mkString("\n").replace(File.separator, "/") // ensure original separators
  }

  def getMockOptions(baseDir: AbsoluteFile): CliOptions =
    getMockOptions(baseDir, baseDir)

  def getMockOptions(
      baseDir: AbsoluteFile,
      workingDir: AbsoluteFile,
      out: PrintStream = Output.NoopStream.printStream,
  ): CliOptions = CliOptions.default.copy(
    baseConfig = ScalafmtConfig.default,
    gitOpsConstructor = _ => new FakeGitOps(baseDir),
    common = CliOptions.default.common
      .copy(cwd = Some(workingDir), out = out, err = out),
  )

  val baseCliOptions: CliOptions =
    getMockOptions(AbsoluteFile(Files.createTempDirectory("base-dir")))
}
