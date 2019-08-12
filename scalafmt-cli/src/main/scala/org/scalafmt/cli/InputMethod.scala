package org.scalafmt.cli

import scala.io.Source

import java.io.File
import java.io.InputStream

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.FileOps

sealed abstract class InputMethod {
  def isSbt: Boolean = filename.endsWith(".sbt")
  def isSc: Boolean = filename.endsWith(".sc")
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
      if (options.testing) {
        if (codeChanged) {
          throw MisformattedFile(
            new File(filename),
            unifiedDiff(
              filename,
              original,
              formatted
            )
          )
          ExitCode.TestError
        } else ExitCode.Ok
      } else if (options.inPlace) {
        if (codeChanged) {
          if (options.list) {
            options.common.out.println(filename)
            ExitCode.TestError
          } else {
            FileOps.writeFile(filename, formatted)(options.encoding)
            ExitCode.Ok
          }
        } else ExitCode.Ok
      } else {
        options.common.out.print(formatted)
        ExitCode.Ok
      }
    }
  }

  def unifiedDiff(
      filename: String,
      original: String,
      revised: String
  ): String = {
    import collection.JavaConverters._
    def jList(string: String) =
      // Predef.augmentString = work around scala/bug#11125 on JDK 11
      java.util.Collections.list(augmentString(string).lines.asJavaEnumeration)
    val a = jList(original)
    val b = jList(revised)
    val diff = difflib.DiffUtils.diff(a, b)
    if (diff.getDeltas.isEmpty) ""
    else {
      difflib.DiffUtils
        .generateUnifiedDiff(
          filename,
          s"$filename-formatted",
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
