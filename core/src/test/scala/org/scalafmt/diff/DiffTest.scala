package org.scalafmt.diff

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatOps
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.logger
import org.scalatest.FunSuite

class DiffTest extends FunSuite with DiffAssertions {
  val code =
    """|package org.scalafmt.diff
       |
       |import scala.meta.Tree
       |
       |object FileDiff {
       |  /** Parses a unified diff into FileDiffs.
       |    * Example commands to produce unified diff:
       |    */
       |  def fromUnified(diff: String): Seq[FileDiff] = {
       |    diff.lines.foreach {
       |      case NewFile(_ , filename) =>
       |        addLastFile()
       |        currentFilename = Some(filename)
       |      case other =>
       |        for {
       |          diffBlock <- DiffBlock.findAllMatchIn(other)
       |          lineCount = Try(diffBlock.group("lineCount").toInt).getOrElse(1)
       |        } {
       |          additions += Addition(startLine, lineCount)
       |        }
       |    }
       |  }
       |
       |  def adjustRanges(ast: Tree, fileDiff: FileDiff): FileDiff = {
       |    val newAdditions =
       |      fileDiff.additions.map { addition =>
       |        ???
       |      }
       |    fileDiff
       |  }
       |}
       |
       |class A()
    """.stripMargin
  import scala.meta._
  val ast = code.parse[Source].get
  val formatOps = new FormatOps(ast, ScalafmtConfig.default)

  ignore("parse") {
    val diff =
      """|--- /dev/null
         |+++ b/core/src/test/scala/org/scalafmt/DiffTest.scala
         |@@ -54 +54,2 @@ class Router(formatOps: FormatOps) {
         |-  import Constants._
         |+  import
         |+  Constants._
         |@@ -57 +58 @@ class Router(formatOps: FormatOps) {
         |-  import TreeOps._
         |+  import  TreeOps._
         |@@ -60,3 +61,6 @@ class Router(formatOps: FormatOps) {
         |-  private def getSplits(formatToken: FormatToken): Seq[Split] = {
         |-    val style = styleMap.at(formatToken)
         |-    val leftOwner = owners(formatToken.left)
         |+  private def getSplits(""".stripMargin

    val expected = List(
      FileDiff("core/src/test/scala/org/scalafmt/DiffTest.scala",
               List(Addition(54, 2), Addition(58, 1), Addition(61, 6))))
    val obtained = FileDiff.fromUnified(diff)
    assert(obtained == expected)
  }

  test("adjust") {
    val fileDiff = FileDiff(
      "Foo.scala",
      Seq(
        Addition(3, 1),
        Addition(7, 1),
        Addition(23, 8),
        Addition(33, 1)
      )
    )
    val ranges =
      FileDiff.getFormatTokenRanges(formatOps.tokens,
                                    fileDiff.additions.map(_.toRange))
    val expected =
      """|diff∙import <-> Tree∙object
         |/** Parses a unified diff into FileDiffs.
         |    * Example commands to produce unified diff:
         |    */∙def <-> /** Parses a unified diff into FileDiffs.
         |    * Example commands to produce unified diff:
         |    */∙def
         |}∙def <-> }∙}
         |}∙class <-> )∙""".stripMargin
    val obtained = ranges.mkString("\n")
    assertNoDiff(obtained, expected)
  }

  test("expand") {
    val ranges: Seq[FormatTokenRange] = Seq(
      (13, 13),
      (17, 17),
      (98, 128),
      (129, 133)
    ).map {
      case (l, r) => FormatTokenRange(formatOps.tokens(l), formatOps.tokens(r))
    }
    ranges.foreach { range =>
      println(FileDiff.expandToEnclosingStatements(range, formatOps))
    }
//    val expanded = FileDiff.expandToEnclosingStatements()

  }

}
