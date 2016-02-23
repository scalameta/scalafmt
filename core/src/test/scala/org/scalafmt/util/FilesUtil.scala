package org.scalafmt.util

object FilesUtil {

  def listFiles(path: String): Vector[String] = {

    def listFilesIter(s: java.io.File): Iterable[String] = {
      val (dirs, files) =
        Option(s.listFiles()).toIterable.flatMap(_.toIterator)
          .partition(_.isDirectory)
      files.map(_.getPath) ++ dirs.flatMap(listFilesIter)
    }
    for {
      f0 <- Option(listFilesIter(new java.io.File(path))).toVector
      filename <- f0
    } yield filename
  }

  /**
    * Reads file from file system or from http url.
    */
  def readFile(filename: String): String = {
    if (filename.startsWith("http")) {
      scala.io.Source.fromURL(filename).mkString
    } else {
      new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths
        .get(filename)))
    }
  }

  def writeFile(filename: String, content: String): Unit = {
    val path = java.nio.file.Paths.get(filename)
    java.nio.file.Files.write(path, content.getBytes)
  }
}
