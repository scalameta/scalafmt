package org.scalafmt.config

class RewriteScala3SettingsTest extends munit.FunSuite {

  import RewriteScala3Settings.Between

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

}
