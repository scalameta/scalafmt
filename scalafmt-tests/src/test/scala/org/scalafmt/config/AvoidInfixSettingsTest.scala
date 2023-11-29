package org.scalafmt.config

class AvoidInfixSettingsTest extends munit.FunSuite {

  Seq(
    Nil,
    Seq("foo"),
    Seq("cross"),
    Seq("cross", "foo")
  ).foreach { extra =>
    test(s"AvoidInfixSettings.forSbtOpt: [${extra.mkString(",")}]") {
      val settings = AvoidInfixSettings.default
        .withExtraExclude(extra.map(AvoidInfixSettings.Filter.apply))
        .getOrElse(AvoidInfixSettings.default)
      settings.forSbtOpt match {
        case None =>
          if (!extra.contains("cross")) fail("forSbtOpt shouldn't be None")
        case Some(sbtSettings) =>
          val expected =
            settings.excludeFilters :+ AvoidInfixSettings.Filter("cross")
          assertEquals(sbtSettings.excludeFilters, expected)
      }
    }
  }

}
