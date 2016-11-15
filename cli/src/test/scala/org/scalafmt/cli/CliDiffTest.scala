package org.scalafmt.cli

class CliDiffTest extends AbstractCliDiffTest {

  check(
    """|/edited.scala
       |object   A {
       |  val x=2
       |}
       |""".stripMargin,
    """|/edited.scala
       |object   A {
       |  val x = 2
       |}
       |""".stripMargin,
    """|--- a/edited.scala
       |+++ b/edited.scala
       |@@ -2,1 +2,1 @@ foo
       |-  val a=1
       |+  val x=1
      """.stripMargin
  )
  check(
    // no diff
    """|/edited.scala
       |object   A {
       |  val x=2  }
       |""".stripMargin,
    """|/edited.scala
       |object   A {
       |  val x=2  }
       |""".stripMargin,
    ""
  )
  check(
    // 2 diffs
    """|/edited.scala
       |object   A  {
       |  val x=1
       |  val y=2
       |  val z=3
       |}
       |""".stripMargin,
    """|/edited.scala
       |object   A  {
       |  val x = 1
       |  val y=2
       |  val z = 3
       |}
       |""".stripMargin,
    """|--- a/edited.scala
       |+++ b/edited.scala
       |@@ -2,1 +2,1 @@ foo
       |-  val a=1
       |+  val x = 1
       |@@ -4,1 +4,1 @@ foo
       |-  val b=3
       |+  val z = 3
    """.stripMargin
  )

  check(
    // leave untouched comments alone
    """|/edited.scala
       | /*
       |   * banana
       |   */
       |object   A  {
       |  val x=1
       |}
       |""".stripMargin,
    """|/edited.scala
       | /*
       |   * banana
       |   */
       |object   A  {
       |  val x = 1
       |}
       |""".stripMargin,
    """|--- a/edited.scala
       |+++ b/edited.scala
       |@@ -5,1 +5,1 @@ foo
       |-  val a=1
       |+  val x = 1
    """.stripMargin
  )
  check(
    // leave untouched comments alone
    """|/edited.scala
       |object A {
       |  function(a, x =>
       |    x+ 2,
       |    b, {
       |    val y = 1
       |    y+2
       |  })
       |}
       |""".stripMargin,
    """|/edited.scala
       |object A {
       |  function(a, x => x + 2, b, {
       |    val y = 1
       |    y+2
       |  })
       |}
       |""".stripMargin,
    """|--- a/edited.scala
       |+++ b/edited.scala
       |@@ -4,1 +3,1 @@ foo
       |-  x+ 3
       |+  x+ 2
    """.stripMargin
  )
  // TODO(olafur) argument indentation
}
