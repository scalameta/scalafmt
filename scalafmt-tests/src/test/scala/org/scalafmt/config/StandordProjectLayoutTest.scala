package org.scalafmt.config

import scala.meta.dialects

import org.scalafmt.sysops.AbsoluteFile

class StandordProjectLayoutTest extends munit.FunSuite {

  import ProjectFiles.Layout.StandardConvention._

  Seq(
    "/prj/src/main/scalaX/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/test/src" -> "scalaX",
    "/prj/src/main/scalaX/src/test" -> "scalaX",
    "/prj/src/main/scalaX/test/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/test/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/test" -> "scalaX",
    "/prj/src/main/scalaX/test/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/test/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/test" -> "scalaX",
    "/prj/src/main/scalaX/test/src/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/test/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/src/test" -> "scalaX",
    "/prj/src/main/scalaX/x/test/src/src" -> "scalaX",
    "/prj/src/main/scalaX/x/test/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/x/test/src/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/x/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/x/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/x/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/src/x" -> "scalaX",
    "/prj/src/main/scalaX/src/x/src/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/x/src/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/x/src/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/src/x/src" -> "scalaX",
    "/prj/src/main/scalaX/src/src/src/src/src/x" -> "scalaX"
  ).foreach { case (path, expectedLang) =>
    test(s"StandardConvention.getLang($path) == $expectedLang") {
      val actualLang = getLang(AbsoluteFile(path))
      assertEquals(actualLang.orNull, expectedLang)
    }
  }

  Seq(
    (s210, "scala-2.10", None),
    (s211, "scala-2.10", s210),
    (s210, "scala-2.11", s211),
    (s211, "scala-2.11", None),
    (s212, "scala-2.11", s211),
    (s211, "scala-2.12", s212),
    (s212, "scala-2.12", None),
    (s213, "scala-2.12", s212),
    (s212, "scala-2.13", s213),
    (s213, "scala-2.13", None),
    (s3, "scala-2.13", s213),
    (s212, "scala-2", None),
    (s213, "scala-2", None),
    (s3, "scala-2", s213),
    (s213, "scala-3", s3),
    (s3, "scala-3", None),
    (nd(dialects.Scala3Future), "scala-3", None)
  ).foreach { case (curDialectOpt, lang, expDialectOpt) =>
    val curName = curDialectOpt.map(_.name).orNull
    val expName = expDialectOpt.map(_.name).orNull
    test(s"StandardConvention.getDialectByLang($lang)($curName) == $expName") {
      val initDialect = curDialectOpt.fold(dialects.Scala3Future)(_.dialect)
      assertEquals(getDialectByLang(lang)(initDialect), expDialectOpt)
    }
  }

}
