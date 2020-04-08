package org.scalafmt.util

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone

import org.scalactic.source.Position
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funsuite.AnyFunSuiteLike

trait DiffAssertions extends AnyFunSuiteLike {

  import LoggerOps._

  case class DiffFailure(
      title: String,
      expected: String,
      obtained: String,
      diff: String
  )(implicit pos: Position)
      extends TestFailedException(
        (_: StackDepthException) =>
          Some(title + "\n" + error2message(obtained, diff)),
        None: Option[Throwable],
        pos
      ) {
    /* fool Intellij, prepend a stack trace element pointing to the test case
     * to allow clicking to the file location; the trick is to show just the
     * short file name under failedCodeFileName, and put the slightly longer
     * version in pos.fileName as a hint in case of multiple matches. */
    // see https://github.com/JetBrains/intellij-scala/blob/ce13d312447ff67404f9359a0543d1bf78c7430a/scala/scala-impl/src/org/jetbrains/plugins/scala/testingSupport/util/scalatest/ScalaTestFailureLocationFilter.java#L76
    override def failedCodeFileName = Some(new File(pos.fileName).getName)
    override lazy val failedCodeStackDepth = 0
    override lazy val getStackTrace =
      new StackTraceElement(pos.fileName, "", "", 0) +: super.getStackTrace
  }

  def error2message(obtained: String, diff: String): String = {
    val sb = new StringBuilder
    if (obtained.length < 1000) sb.append(s"""
         #${header("Obtained")}
         #${trailingSpace(obtained)}
         """.stripMargin('#'))
    sb.append(s"""
         #${header("Diff")}
         #${trailingSpace(diff)}
         """.stripMargin('#'))
    sb.toString()
  }

  def assertNoDiffOrPrintExpected(
      obtained: String,
      expected: String,
      title: String = ""
  )(implicit source: Position): Unit = {
    try assertNoDiff(obtained, expected, title)
    catch {
      case ex: Exception =>
        obtained.linesIterator.toList match {
          case head +: tail =>
            println("    \"\"\"|" + head)
            tail.foreach(line => println("       |" + line))
          case head +: Nil =>
            println(head)
          case Nil =>
            println("obtained is empty")
        }
        throw ex
    }
  }

  def assertNoDiff(obtained: String, expected: String, title: String = "")(
      implicit pos: Position
  ): Unit = {
    val diff = compareContents(expected, obtained)
    if (diff.nonEmpty)
      throw DiffFailure(title, expected, obtained, diff)
    assert(
      obtained.length == expected.length,
      ". Is there a redundant/missing trailing newline?"
    )
  }

  def assertNoDiff(file: AbsoluteFile, expected: String)(
      implicit pos: Position
  ): Unit =
    assertNoDiff(FileOps.readFile(file.jfile), expected)

  def trailingSpace(str: String): String = str.replaceAll(" \n", "∙\n")

  def compareContents(original: String, revised: String): String =
    compareContents(original.trim.split("\n"), revised.trim.split("\n"))

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
