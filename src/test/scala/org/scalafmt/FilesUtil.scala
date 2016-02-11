package org.scalafmt

object FilesUtil {

  def listFiles(path: String): Vector[String] = {

    def listFilesIter(s: java.io.File): Iterator[String] = {
      val (dirs, files) =
        Option(s.listFiles()).toIterator.flatMap(_.toIterator)
          .partition(_.isDirectory)
      files.map(_.getPath) ++ dirs.flatMap(listFilesIter)
    }
    for {
      f0 <- Option(listFilesIter(new java.io.File(path))).toVector
      filename <- f0
    } yield filename
  }

  def readFile(filename: String): String =
    new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths
              .get(filename)))

  def writeFile(filename: String, content: String): Unit = {
    val path = java.nio.file.Paths.get(filename)
    java.nio.file.Files.write(path, content.getBytes)
  }
}
