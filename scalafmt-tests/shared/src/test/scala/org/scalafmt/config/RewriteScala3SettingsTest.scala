package org.scalafmt
package config

class RewriteScala3SettingsTest extends SharedFunSuiteBase {

  import RewriteScala3Settings._

  private def hocon(str: String): ScalafmtConfig = ScalafmtConfig
    .fromHoconString(str).get

  Seq(
    Between(),
    Between(min = -2),
    Between(min = 1, max = 0),
    Between(min = Int.MaxValue, max = Int.MaxValue - 1),
  ).foreach(bw =>
    test(s"RewriteScala3Settings.Between: not enabled [$bw}]")(
      assert(!bw.enabled, s"Between should not be enabled for $bw"),
    ),
  )

  Seq(
    (Between(max = 1), Between(max = 2), true),
    (Between(min = 1), Between(min = 2), true),
    (Between(min = 1), Between(max = 1), true),
    (Between(min = 2), Between(max = 1), false),
    (Between(min = 1, max = 2), Between(max = 0), false),
    (Between(min = 1, max = 2), Between(max = 1), true),
    (Between(min = 1, max = 2), Between(max = 2), true),
    (Between(min = 1, max = 2), Between(max = 3), true),
    (Between(min = 1, max = 2), Between(min = 1), true),
    (Between(min = 1, max = 2), Between(min = 2), true),
    (Between(min = 1, max = 2), Between(min = 3), false),
    (Between(min = 1, max = 2), Between(min = 1, max = 0), false),
    (Between(min = 1, max = 2), Between(min = 2, max = 1), true),
    (Between(min = 1, max = 2), Between(min = 3, max = 2), false),
  ).foreach { case (lt, rt, overlaps) =>
    test(s"RewriteScala3Settings.Between: overlaps [$lt, $rt}]")(
      assert(
        lt.overlaps(rt) == overlaps,
        s"Between should overlap=$overlaps: $lt and $rt",
      ),
      assert(
        rt.overlaps(lt) == overlaps,
        s"Between should overlap=$overlaps: $lt and $rt",
      ),
    )
  }

  Seq(
    (Between(min = 1, max = 5), Between(), Between(min = 1, max = 5)),
    (Between(), Between(min = 1), Between()),
    (Between(min = 1, max = 5), Between(max = 2), Between(min = 3, max = 5)),
    (Between(min = 1, max = 5), Between(min = 4), Between(min = 1, max = 3)),
    (
      Between(min = 1, max = 5),
      Between(min = 2, max = 4),
      Between(min = 5, max = 1),
    ),
    (Between(max = 5), Between(max = 2), Between(min = 3, max = 5)),
    (Between(min = 1), Between(min = 3), Between(min = 1, max = 2)),
    (Between(min = 0), Between(min = 1), Between(min = 0, max = 0)),
    (Between(min = 0), Between(min = 0), Between()),
  ).foreach { case (lt, rt, excluded) =>
    test(s"RewriteScala3Settings.Between: exclude [$lt, $rt]")(
      assertEquals(lt.exclude(rt), excluded),
    )
  }

  test("RewriteScala3Settings: insert with an unset remove") {
    val base = hocon("runner.dialect = scala3")
    def styleFor(ob: RemoveOptionalBraces) = {
      val rewrite = base.rewrite
      base.copy(rewrite =
        rewrite.copy(scala3 = rewrite.scala3.copy(optionalBraces = ob)),
      )
    }
    val src = "def f: Int =\n  1\n\n  2\n"
    def formatTwice(ob: RemoveOptionalBraces) = {
      val style = styleFor(ob)
      val once = Scalafmt.formatCode(src, style).get
      (once, Scalafmt.formatCode(once, style).get)
    }
    val ob = RemoveOptionalBraces(insert =
      Some(BracesFilters(blankGaps = Between(min = 1))),
    )
    // an unset `remove` reads as never once `insert` is set, not as always
    val (once, twice) = formatTwice(ob)
    assertEquals(twice, once)
  }

  private val gapsFrom1 = BracesFilters(blankGaps = Between(min = 1))
  private val spanTo5 = BracesFilters(span = Between(max = 5))
  Seq(
    // preferInsert, insert, remove, insertFilters, removeFilters
    (true, None, None, None, None),
    (true, None, Some(spanTo5), None, Some(spanTo5)),
    (true, Some(gapsFrom1), None, Some(gapsFrom1), Some(BracesFilters.disabled)),
    (
      true,
      Some(gapsFrom1),
      Some(spanTo5),
      Some(gapsFrom1),
      Some(spanTo5.exclude(gapsFrom1)),
    ),
    (false, None, None, None, None),
    (false, None, Some(spanTo5), None, Some(spanTo5)),
    (false, Some(gapsFrom1), None, Some(gapsFrom1), Some(BracesFilters.disabled)),
    (
      false,
      Some(gapsFrom1),
      Some(spanTo5),
      Some(gapsFrom1.exclude(spanTo5)),
      Some(spanTo5),
    ),
  ).foreach { case (preferInsert, insert, remove, insFilters, remFilters) =>
    test(s"RewriteScala3Settings: filters [preferInsert=$preferInsert, $insert, $remove]") {
      val obtained = RemoveOptionalBraces(
        preferInsert = preferInsert,
        insert = insert,
        remove = remove,
      )
      assertEquals(obtained.insertFilters, insFilters)
      assertEquals(obtained.removeFilters, remFilters)
    }
  }

  Seq( // presets
    "common",
    "true",
    "false",
  ).foreach(preset =>
    test(s"RewriteScala3Settings: preset ranges [$preset]") {
      val rob = hocon(s"rewrite.scala3.preset = $preset").rewrite.scala3
        .optionalBraces
      val overlaps = rob.insertFilters.exists(i =>
        rob.removeFilters.exists(r =>
          i.span.overlaps(r.span) || i.blankGaps.overlaps(r.blankGaps),
        ),
      )
      assert(!overlaps, s"preset ranges overlap: $rob")
    },
  )

}
