package org.scalafmt.cli

import java.io.File
import java.io.InputStream
import java.io.PrintStream

import org.scalafmt.config.ProjectFiles
import org.scalafmt.config.ScalafmtConfig

object CliOptions {
  val default = CliOptions()
}
case class CommonOptions(
    workingDirectory: String = System.getProperty("user.dir"),
    out: PrintStream = System.out,
    in: InputStream = System.in,
    err: PrintStream = System.err
)

case class CliOptions(
    config: ScalafmtConfig = ScalafmtConfig.default,
    range: Set[Range] = Set.empty[Range],
    inPlace: Boolean = false,
    testing: Boolean = false,
    stdIn: Boolean = false,
    assumeFilename: String = "foobar.scala", // used when read from stdin
    migrate: Option[File] = None,
    common: CommonOptions = CommonOptions()
) {
  require(!(inPlace && testing), "inPlace and testing can't both be true")
  def withFiles(files: Seq[File]): CliOptions = {
    this.copy(
      config = config.copy(
        project = config.project.copy(
          files = files.map(_.getPath)
        )
      )
    )
  }
}
