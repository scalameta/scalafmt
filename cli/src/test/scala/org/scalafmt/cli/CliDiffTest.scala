package org.scalafmt.cli

class X extends AbstractCliDiffTest {
}

class CliDiffTest extends AbstractCliDiffTest {
  check(
    """|/version.sbt
       |version in ThisBuild :=  "0.1.5-SNAPSHOT"
       |/.scalafmt.conf
       |runner.fatalWarnings = true
       |project.git = true
       |""".stripMargin,
    """|/.scalafmt.conf
       |runner.fatalWarnings = true
       |project.git = true
       |
       |/version.sbt
       |version in ThisBuild := "0.1.5-SNAPSHOT"
       |""".stripMargin,
    """|--- a/version.sbt
       |+++ b/version.sbt
       |@@ -1 +1 @@
       |-version in ThisBuild := "0.1.5-SNAPSHOT"
       |+version in ThisBuild := "0.1.6-SNAPSHOT"
    """.stripMargin
  )

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
