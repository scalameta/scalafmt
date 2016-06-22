package org.scalafmt.util

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone

import org.scalatest.FunSuiteLike
import org.scalatest.exceptions.TestFailedException

trait DiffAssertions extends FunSuiteLike {
  import LoggerOps._

  case class DiffFailure(title: String,
                         expected: String,
                         obtained: String,
                         diff: String)
      extends TestFailedException(
          title + "\n" + error2message(obtained, expected),
          1)

  def error2message(obtained: String, expected: String): String = {
    val sb = new StringBuilder
    if (obtained.length < 1000) {
      sb.append(s"""
         #${header("Obtained")}
         #${trailingSpace(obtained)}
         """.stripMargin('#'))
    }
    sb.append(s"""
       #${header("Diff")}
       #${trailingSpace(compareContents(obtained, expected))}
         """.stripMargin('#'))
    sb.toString()
  }

  def assertNoDiff(obtained: String,
                   expected: String,
                   title: String = ""): Boolean = {
    val result = compareContents(obtained, expected)
    if (result.isEmpty) true
    else {
      throw DiffFailure(title, expected, obtained, result)
    }
  }

  def trailingSpace(str: String): String = str.replaceAll(" \n", "∙\n")

  def compareContents(original: String, revised: String): String = {
    compareContents(original.trim.split("\n"), revised.trim.split("\n"))
  }

  def compareContents(original: Seq[String], revised: Seq[String]): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
            "original",
            "revised",
            original.asJava,
            diff,
            1
        )
        .asScala
        .drop(3)
        .mkString("\n")
  }

  def fileModificationTimeOrEpoch(file: File): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss Z")
    if (file.exists) format.format(new Date(file.lastModified()))
    else {
      format.setTimeZone(TimeZone.getTimeZone("UTC"))
      format.format(new Date(0L))
    }
  }
}
