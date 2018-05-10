package org.scalafmt

import metaconfig.Conf
import metaconfig.ConfEncoder
import org.scalafmt.config.Config
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite


class ToHoconSuite extends FunSuite with DiffAssertions {
  implicit val encoder = ConfEncoder.instance[Conf](identity)
  def check(original: Conf, expected: String): Unit = {
    test(original.toString()) {
      val obtained = Config.toHocon(original).render(10)
      println(obtained)
      assertNoDiff(obtained, expected)
    }
  }
  check(
    Conf.Obj(
      "a" -> Conf.Bool(true),
      "b" -> Conf.Null(),
      "c" -> Conf.Num(1),
      "d" -> Conf.Lst(Conf.Str("2"), Conf.Str("")),
      "e" -> Conf.Obj("f" -> Conf.Num(3)),
      "f.g" -> Conf.Num(2),
    ),
    """|a = true
       |b = null
       |c = 1
       |d = [
       |  "2"
       |  ""
       |]
       |e.f = 3
       |"f.g" = 2
       |""".stripMargin.trim
  )

  check(
    Conf.Obj(
      "a" -> Conf.Lst(Conf.Str("b.c"))
    ),
    """
      |a = [
      |  "b.c"
      |]
    """.stripMargin.trim
  )

}
