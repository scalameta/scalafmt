package org.scalafmt.community

import org.scalafmt.config._

import java.nio.file._

import scala.concurrent.duration
import scala.sys.process._

import munit.FunSuite

abstract class CommunitySuite extends FunSuite {

  import TestHelpers._

  override val munitTimeout = new duration.FiniteDuration(5, duration.MINUTES)

  protected def builds: Seq[CommunityBuild]

  for {
    build <- builds
    (k, v) <- TestStyles.styles
    if build.styles.isEmpty || build.styles.contains(k) == build.stylesIncluded
  } {
    val prefix = s"[ref ${build.commit}, style $k]"
    val style: ScalafmtConfig = v.withDialect(NamedDialect(build.dialect))
    test(s"community-build: ${build.name} $prefix")(check(k)(build, style))
  }

  private def check(
      styleName: String,
  )(implicit build: CommunityBuild, style: ScalafmtConfig): Unit = {
    val folder = fetchCommunityBuild

    val stats = checkFilesRecursive(styleName, folder.toAbsolutePath)
      .getOrElse(TestStats.init)
    val timePer1KLines = Math
      .round(stats.timeTaken / (stats.linesParsed / 1000.0))

    println("--------------------------")
    println(s"${build.name} [ref=${build.commit}] [style=$styleName]")
    println(s"Files parsed correctly ${stats.checkedFiles - stats.errors}")
    println(s"Files errored: ${stats.errors}")
    println(s"Time taken: ${stats.timeTaken}ms")
    if (stats.linesParsed < 1000) println(s"Lines parsed: ${stats.linesParsed}")
    else println(s"Lines parsed: ~${stats.linesParsed / 1000}k")
    println(s"Parsing speed per 1k lines ===> $timePer1KLines ms/1klines")
    println("--------------------------")
    stats.lastError.foreach(e => throw e)

    assertEquals(stats.errors, 0)
    assertEquals(
      stats.checkedFiles,
      build.checkedFiles * 2,
      s"expected ${stats.checkedFiles / 2} per run",
    )
  }

  private def fetchCommunityBuild(implicit build: CommunityBuild): Path = {
    if (!Files.exists(communityDirectory)) Files
      .createDirectory(communityDirectory)

    val log = new StringBuilder
    val logger = ProcessLogger(s => log.append(s).append('\n'))

    def runCmd(cmd: String, what: => String): Unit = {
      val result: Int = cmd.!(logger)
      assertEquals(
        clue(result),
        0,
        s"Community build ${build.name}: $what failed:\n$log",
      )
      log.clear()
    }

    val folderPath = communityDirectory.resolve(build.name)
    val folder = folderPath.toString

    if (!Files.exists(folderPath)) runCmd(
      s"git clone --depth=1 --no-single-branch ${build.giturl} $folder",
      "cloning",
    )

    val ref = build.commit

    runCmd(s"git -C $folder fetch --depth=1 origin $ref", s"fetching [ref=$ref]")

    runCmd(
      s"git -C $folder checkout -f -B ref-$ref $ref",
      s"checking out [ref=$ref]",
    )

    folderPath
  }

}
