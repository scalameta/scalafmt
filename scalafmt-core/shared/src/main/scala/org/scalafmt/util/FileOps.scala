package org.scalafmt.util

import scala.io.Codec
import java.io._

object FileOps {

  def makeAbsolute(workingDir: File)(file: File): File =
    if (file.isAbsolute) file
    else new File(workingDir, file.getPath)

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
    // Prefer this to inefficient Source.fromFile.
    val sb = new StringBuilder
    val br = new BufferedReader(
      new InputStreamReader(new FileInputStream(file), codec.charSet)
    )
    try {
      var line = ""
      while ({
        line = br.readLine()
        line != null
      }) {
        sb.append(line)
        sb.append("\n")
      }
    } finally {
      br.close()
    }
    sb.toString()
  }

  def getFile(path: String*): File = {
    new File(path.mkString(File.separator))
  }

  def writeFile(file: AbsoluteFile, content: String)(
      implicit codec: Codec
  ): Unit =
    writeFile(file.jfile, content)

  def writeFile(file: File, content: String)(implicit codec: Codec): Unit = {
    // For java 6 compatibility we don't use java.nio.
    val bw = new BufferedWriter(
      new OutputStreamWriter(new FileOutputStream(file), codec.charSet)
    )
    try {
      bw.write(content)
    } finally {
      bw.close()
    }

  }
  def writeFile(filename: String, content: String)(
      implicit codec: Codec
  ): Unit = {
    writeFile(new File(filename), content)
  }
}
