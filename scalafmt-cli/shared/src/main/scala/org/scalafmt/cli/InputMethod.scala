package org.scalafmt.cli

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.FileOps
import org.scalafmt.sysops.PlatformCompat
import org.scalafmt.sysops.PlatformFileOps
import org.scalafmt.sysops.PlatformRunOps

import java.io.InputStream
import java.nio.file.Path
import java.nio.file.Paths

import scala.concurrent.Future

import munit.{diff => difflib}

sealed abstract class InputMethod {
  def readInput(options: CliOptions): Future[String]
  def path: Path

  protected def print(text: String, options: CliOptions): Unit
  protected def list(options: CliOptions): Unit
  protected def overwrite(text: String, options: CliOptions): Future[Unit]

  final def write(
      formatted: String,
      original: String,
      options: CliOptions,
  ): Future[ExitCode] = {
    def codeChanged = formatted != original
    def exitCode = if (codeChanged) options.exitCodeOnChange else ExitCode.Ok
    options.writeMode match {
      case WriteMode.Stdout => print(formatted, options); exitCode.future
      case _ if !codeChanged => ExitCode.Ok.future
      case WriteMode.List => list(options); options.exitCodeOnChange.future
      case WriteMode.Override => overwrite(formatted, options).map(_ =>
          options.exitCodeOnChange,
        )(PlatformRunOps.parasiticExecutionContext)
      case WriteMode.Test =>
        val pathStr = path.toString
        val diff = InputMethod.unifiedDiff(pathStr, original, formatted)
        val msg =
          if (diff.nonEmpty) diff
          else s"--- +$pathStr\n    => modified line endings only"
        Future.failed(MisformattedFile(path, msg))
      case _ => options.exitCodeOnChange.future
    }
  }

}

object InputMethod {

  case class StdinCode(filename: String, inputStream: InputStream)
      extends InputMethod {
    override def path: Path = Paths.get(filename)

    def readInput(options: CliOptions): Future[String] = {
      val stdin = (inputStream eq null) || (inputStream eq System.in)
      if (stdin) PlatformFileOps.readStdinAsync
      else Future.successful(FileOps.readInputStream(inputStream))
    }

    override protected def print(text: String, options: CliOptions): Unit =
      options.common.out.print(text)

    override protected def overwrite(
        text: String,
        options: CliOptions,
    ): Future[Unit] = Future.successful(print(text, options))

    override protected def list(options: CliOptions): Unit = options.common.out
      .println(filename)
  }

  case class FileContents(file: AbsoluteFile) extends InputMethod {
    override def path = file.path
    def readInput(options: CliOptions): Future[String] = PlatformFileOps
      .readFileAsync(path)(options.encoding)

    override protected def print(text: String, options: CliOptions): Unit =
      options.common.out.print(text)

    override protected def overwrite(
        text: String,
        options: CliOptions,
    ): Future[Unit] = PlatformFileOps
      .writeFileAsync(file.path, text)(options.encoding)

    override protected def list(options: CliOptions): Unit = options.common.out
      .println(PlatformCompat.relativize(options.cwd, file))
  }

  def unifiedDiff(filename: String, original: String, revised: String): String = {
    import org.scalafmt.CompatCollections.JavaConverters._
    @inline
    def noEol(ch: Char) = ch != '\n' && ch != '\r'
    def jList(code: String, addEol: Boolean) = {
      val last = if (addEol) Iterator.single("") else Iterator.empty
      (code.linesIterator ++ last).toList.asJava
    }
    val a = jList(original, false)
    // formatted always has EOL
    val b = jList(revised, original.isEmpty || noEol(original.last))
    val diff = difflib.DiffUtils.diff(a, b)
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils
      .generateUnifiedDiff(s"a$filename", s"b$filename", a, diff, 1).iterator()
      .asScala.mkString("\n")
  }
}
