package org.scalafmt

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone

import org.scalatest.exceptions.TestFailedException

object DiffUtil extends ScalaFmtLogger {

  implicit class DiffExtension(obtained: String) {
    def diff(expected: String): Boolean = {
      val result = compareContents(obtained, expected)
      if (result.isEmpty) true
      else throw new TestFailedException(
        s"""
           |${header("Obtained")}
           |$obtained
           |
           |${header("Diff")}
           |$result
         """.stripMargin, 1)
    }
  }

  def compareContents(original: String, revised: String): String = {
    compareContents(original.split("\n"), revised.split("\n"))
  }

  def compareContents(original: Seq[String],
                      revised: Seq[String]): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else difflib.DiffUtils.generateUnifiedDiff(
      "original", "revised",original.asJava, diff, 1).asScala.drop(3).mkString("\n")
  }

  def fileModificationTimeOrEpoch(file: File): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss Z")
    if (file.exists)
      format.format(new Date(file.lastModified()))
    else {
      format.setTimeZone(TimeZone.getTimeZone("UTC"))
      format.format(new Date(0L))
    }
  }
}
