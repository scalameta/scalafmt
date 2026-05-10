package org.scalafmt.cli

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.RegexCompat
import org.scalafmt.sysops._

import java.io.{File, PrintStream}

object FileTestOps {

  /** The inverse of [[dir2string]]. Given a string representation creates the
    * necessary files/directories with respective file contents.
    */
  def string2dir(layout: String): AbsoluteFile = {
    val root = AbsoluteFile(PlatformFileOps.mkdtemp("root"))
    RegexCompat.splitByBeforeTextMatching(layout, "\n/").foreach { row =>
      val strippedRow = row.stripPrefix("\n")
      val eolIdx = strippedRow.indexOf('\n')
      val path = strippedRow.substring(0, eolIdx)
      val file = root / path.stripPrefix("/")
      file.parent.mkdirs()
      val contents = strippedRow.substring(eolIdx + 1)
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
          |${PlatformFileOps.readFile(path)}""".stripMargin,
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
    getMockOptions(AbsoluteFile(PlatformFileOps.mkdtemp("base-dir")))
}
