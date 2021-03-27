package org.scalafmt.cli

import scala.io.Source

import java.io.File
import java.io.InputStream

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.FileOps

sealed abstract class InputMethod {
  def readInput(options: CliOptions): String
  def filename: String
  def write(formatted: String, original: String, options: CliOptions): ExitCode
}

object InputMethod {

  object StdinCode {
    def apply(assumeFilename: String, inputStream: InputStream): StdinCode = {
      StdinCode.apply(
        assumeFilename,
        Source.fromInputStream(inputStream).getLines().mkString("\n")
      )
    }
  }
  case class StdinCode(filename: String, input: String) extends InputMethod {
    def readInput(options: CliOptions): String = input
    override def write(
        code: String,
        original: String,
        options: CliOptions
    ): ExitCode = {
      options.common.out.print(code)
      ExitCode.Ok
    }
  }
  case class FileContents(file: AbsoluteFile) extends InputMethod {
    override def filename = file.path
    def readInput(options: CliOptions): String =
      FileOps.readFile(filename)(options.encoding)
    override def write(
        formatted: String,
        original: String,
        options: CliOptions
    ): ExitCode = {
      val codeChanged = formatted != original
      if (options.writeMode == WriteMode.Stdout)
        options.common.out.print(formatted)
      else if (codeChanged)
        options.writeMode match {
          case WriteMode.Test =>
            throw MisformattedFile(
              new File(filename),
              unifiedDiff(filename, original, formatted)
            )
          case WriteMode.Override =>
            FileOps.writeFile(filename, formatted)(options.encoding)
          case WriteMode.List =>
            options.common.out.println(
              options.common.workingDirectory.jfile
                .toURI()
                .relativize(file.jfile.toURI())
            )
          case _ =>
        }
      if (options.error && codeChanged) ExitCode.TestError else ExitCode.Ok
    }
  }

  def unifiedDiff(
      filename: String,
      original: String,
      revised: String
  ): String = {
    import org.scalafmt.CompatCollections.JavaConverters._
    def jList(string: String) =
      java.util.Collections.list(string.linesIterator.asJavaEnumeration)
    val a = jList(original)
    val b = jList(revised)
    val diff = difflib.DiffUtils.diff(a, b)
    if (diff.getDeltas.isEmpty) ""
    else {
      difflib.DiffUtils
        .generateUnifiedDiff(
          s"a$filename",
          s"b$filename",
          a,
          diff,
          1
        )
        .iterator()
        .asScala
        .mkString("\n")
    }
  }
}
