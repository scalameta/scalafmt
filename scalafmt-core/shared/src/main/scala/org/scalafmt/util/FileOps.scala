package org.scalafmt.util

import java.io._
import java.nio.file.{Files, LinkOption, Path, Paths}
import scala.io.Codec

object FileOps {

  def makeAbsolute(workingDir: File)(file: File): File =
    if (file.isAbsolute) file
    else new File(workingDir, file.getPath)

  def isRegularFile(file: File): Boolean =
    Files.isRegularFile(file.toPath, LinkOption.NOFOLLOW_LINKS)

  def listFiles(path: String): Vector[String] = {
    listFiles(new File(path))
  }

  def listFiles(file: AbsoluteFile): Vector[AbsoluteFile] = {
    listFiles(file.jfile).map(x => AbsoluteFile.fromFile(new File(x), file))
  }

  def listFiles(file: File): Vector[String] = {
    if (file.isFile) {
      Vector(file.getAbsolutePath)
    } else {
      def listFilesIter(s: File): Iterable[String] = {
        val (dirs, files) = Option(s.listFiles()).toIterable
          .flatMap(_.toIterator)
          .partition(_.isDirectory)
        files.map(_.getPath) ++ dirs.flatMap(listFilesIter)
      }
      for {
        f0 <- Option(listFilesIter(file)).toVector
        filename <- f0
      } yield filename
    }
  }

  // TODO(olafur) allow user to specify encoding through CLI.
  /**
    * Reads file from file system or from http url.
    */
  def readFile(filename: String)(implicit codec: Codec): String = {
    if (filename matches "https?://.*") {
      scala.io.Source.fromURL(filename)("UTF-8").getLines().mkString("\n")
    } else {
      readFile(new File(filename))
    }
  }

  def readFile(file: AbsoluteFile)(implicit codec: Codec): String = {
    readFile(file.jfile)
  }

  def readFile(file: File)(implicit codec: Codec): String = {
    new String(Files.readAllBytes(file.toPath), codec.charSet)
  }

  def getFile(path: String*): File = {
    new File(path.mkString(File.separator))
  }

  def writeFile(file: AbsoluteFile, content: String)(
      implicit codec: Codec
  ): Unit = {
    writeFile(file.jfile, content)
  }

  def writeFile(file: File, content: String)(implicit codec: Codec): Unit = {
    writeFile(file.toPath, content)
  }

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = {
    val bytes = content.getBytes(codec.charSet)
    Files.write(path, bytes)
  }

  def writeFile(filename: String, content: String)(
      implicit codec: Codec
  ): Unit = {
    writeFile(Paths.get(filename), content)
  }
}
