package org.scalafmt.util

import java.io._
import java.nio.file.{Files, LinkOption, Path, Paths}
import scala.io.Codec

object FileOps {

  @inline
  def getLastModifiedMsec(file: Path): Long =
    Files.getLastModifiedTime(file, LinkOption.NOFOLLOW_LINKS).toMillis

  @inline
  def isDirectory(file: Path): Boolean =
    Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS)

  @inline
  def isRegularFile(file: Path): Boolean =
    Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS)

  def listFiles(path: String): Seq[Path] =
    listFiles(new File(path))

  def listFiles(file: File): Seq[Path] =
    if (file.isFile) {
      Seq(file.toPath)
    } else {
      def listFilesIter(s: File): Iterable[Path] = {
        val (dirs, files) = Option(s.listFiles()).toIterable
          .flatMap(_.toIterator)
          .partition(_.isDirectory)
        files.map(_.toPath) ++ dirs.flatMap(listFilesIter)
      }
      for {
        f0 <- Option(listFilesIter(file)).toVector
        filename <- f0
      } yield filename
    }

  // TODO(olafur) allow user to specify encoding through CLI.
  /** Reads file from file system or from http url.
    */
  def readFile(filename: String)(implicit codec: Codec): String = {
    if (filename matches "https?://.*") {
      scala.io.Source.fromURL(filename)("UTF-8").getLines().mkString("\n")
    } else {
      readFile(Paths.get(filename))
    }
  }

  def readFile(file: Path)(implicit codec: Codec): String = {
    new String(Files.readAllBytes(file), codec.charSet)
  }

  def getFile(path: String*): Path =
    Paths.get(path.head, path.tail: _*)

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = {
    val bytes = content.getBytes(codec.charSet)
    Files.write(path, bytes)
  }

  def writeFile(filename: String, content: String)(implicit
      codec: Codec
  ): Unit = {
    writeFile(Paths.get(filename), content)
  }

  @inline
  def isMarkdown(filename: String): Boolean = filename.endsWith(".md")

  @inline
  def isAmmonite(filename: String): Boolean = filename.endsWith(".sc")

  @inline
  def isSbt(filename: String): Boolean = filename.endsWith(".sbt")

}
