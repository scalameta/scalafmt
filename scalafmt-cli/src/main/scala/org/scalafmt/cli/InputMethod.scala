package org.scalafmt.cli

import scala.io.Source

import java.io.InputStream
import java.nio.file.{Path, Paths}

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.sysops.AbsoluteFile

sealed abstract class InputMethod {
  def readInput(options: CliOptions): String
  def path: Path

  protected def print(text: String, options: CliOptions): Unit
  protected def list(options: CliOptions): Unit
  protected def overwrite(text: String, options: CliOptions): Unit

  final def write(
      formatted: String,
      original: String,
      options: CliOptions
  ): ExitCode = {
    val codeChanged = formatted != original
    if (options.writeMode == WriteMode.Stdout) print(formatted, options)
    else if (codeChanged)
      options.writeMode match {
        case WriteMode.Test =>
          val pathStr = path.toString
          val diff = InputMethod.unifiedDiff(pathStr, original, formatted)
          val msg =
            if (diff.nonEmpty) diff
            else s"--- +$pathStr\n    => modified line endings only"
          throw MisformattedFile(path, msg)
        case WriteMode.Override => overwrite(formatted, options)
        case WriteMode.List => list(options)
        case _ =>
      }
    if (options.error && codeChanged) ExitCode.TestError else ExitCode.Ok
  }

}

object InputMethod {

  object StdinCode {
    def apply(assumeFilename: String, inputStream: InputStream): StdinCode = {
      StdinCode.apply(
        assumeFilename,
        Source.fromInputStream(inputStream).mkString
      )
    }
  }
  case class StdinCode(filename: String, input: String) extends InputMethod {
    override def path: Path = Paths.get(filename)

    def readInput(options: CliOptions): String = input

    override protected def print(text: String, options: CliOptions): Unit =
      options.common.out.print(text)

    override protected def overwrite(text: String, options: CliOptions): Unit =
      print(text, options)

    override protected def list(options: CliOptions): Unit =
      options.common.out.println(filename)
  }

  case class FileContents(file: AbsoluteFile) extends InputMethod {
    override def path = file.path
    def readInput(options: CliOptions): String =
      file.readFile(options.encoding)

    override protected def print(text: String, options: CliOptions): Unit =
      options.common.out.print(text)

    override protected def overwrite(text: String, options: CliOptions): Unit =
      file.writeFile(text)(options.encoding)

    override protected def list(options: CliOptions): Unit = {
      val cwd = options.cwd.toUri
      options.common.out.println(cwd.relativize(file.toUri))
    }
  }

  def unifiedDiff(
      filename: String,
      original: String,
      revised: String
  ): String = {
    import org.scalafmt.CompatCollections.JavaConverters._
    @inline def noEol(ch: Char) = ch != '\n' && ch != '\r'
    def jList(code: String, addEol: Boolean) = {
      val last = if (addEol) Iterator.single("") else Iterator.empty
      (code.linesIterator ++ last).toList.asJava
    }
    val a = jList(original, false)
    // formatted always has EOL
    val b = jList(revised, original.isEmpty || noEol(original.last))
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
