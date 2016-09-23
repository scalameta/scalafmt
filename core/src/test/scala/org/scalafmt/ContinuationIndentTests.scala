package org.scalafmt

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.DiffTest
import org.scalafmt.util.HasTests
import org.scalatest.FunSuite
import scala.meta.Tree
import scala.meta.parsers.Parse

class ContinuationIndentTests
    extends FunSuite
    with HasTests
    with DiffAssertions {

  val defnSite3callSite5 = """
<<< #143
def foo(
a: Int,
b: Int
): Int = {
  function(
2,
3
)
}
>>>
def foo(
   a: Int,
   b: Int
): Int = {
  function(
       2,
       3
  )
}
""".replace("'''", "\"\"\"")

  override val tests =
    parseDiffTests(defnSite3callSite5, "default/continuationIndent.stat")

  testsToRun.foreach(runTest(run))

  val style = ScalafmtStyle.default.copy(
    continuationIndent = ContinuationIndent(5, 3)
  )

  def run(t: DiffTest, parse: Parse[_ <: Tree]): Unit = {
    val runner = scalafmtRunner.withParser(parse)
    val formatted = Scalafmt.format(t.original, style, runner).get
    saveResult(t, formatted, t.only)
    assertNoDiff(formatted, t.expected)
  }
}
