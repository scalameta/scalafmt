package org.scalafmt.util

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import org.scalafmt.internal.ScalaFmtLogger

object FilesUtil extends ScalaFmtLogger {

  def listFiles(path: String): Vector[String] = {
    listFiles(new File(path))
  }

  def listFiles(file: File): Vector[String] = {
    if (Files.isRegularFile(Paths.get(file.toURI))) {
      Vector(file.getAbsolutePath)
    } else {

      def listFilesIter(s: File): Iterable[String] = {
        val (dirs, files) =
          Option(s.listFiles()).toIterable.flatMap(_.toIterator)
            .partition(_.isDirectory)
        files.map(_.getAbsolutePath) ++ dirs.flatMap(listFilesIter)
      }
      for {
        f0 <- Option(listFilesIter(file)).toVector
        filename <- f0
      } yield filename
    }
  }

  /**
    * Reads file from file system or from http url.
    */
  def readFile(filename: String): String = {
    if (filename.startsWith("http")) {
      scala.io.Source.fromURL(filename).mkString
    } else {
      new String(Files.readAllBytes(Paths.get(filename)))
    }
  }

  def writeFile(filename: String, content: String): Unit = {
    Files.write(Paths.get(filename), content.getBytes)
  }
}
