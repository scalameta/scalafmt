package org.scalafmt.cli

import scala.io.Source

import java.io.File
import java.io.InputStream

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.util.FileOps

sealed abstract class InputMethod {
  def isSbt = filename.endsWith(".sbt")
  def readInput: String
  def filename: String
  def write(formatted: String, original: String, options: CliOptions): Unit
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
  case class StdinCode(filename: String, readInput: String)
      extends InputMethod {
    override def write(code: String,
                       original: String,
                       options: CliOptions): Unit = {
      options.common.out.println(code)
    }
  }
  case class FileContents(filename: String) extends InputMethod {
    def readInput: String = FileOps.readFile(filename)
    override def write(formatted: String,
                       original: String,
                       options: CliOptions): Unit = {
      {
        val codeChanged = formatted != original
        if (options.testing && codeChanged) {
          throw MisformattedFile(new File(filename))
        } else if (options.inPlace) {
          if (codeChanged) FileOps.writeFile(filename, formatted)
          else Unit
        } else {
          options.common.out.println(formatted)
        }
      }
    }
  }
}
