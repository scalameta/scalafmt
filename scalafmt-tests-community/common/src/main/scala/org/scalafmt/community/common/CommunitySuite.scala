package org.scalafmt.community.common

import org.scalafmt.config._
import org.scalafmt.sysops.OsSpecific

import java.nio.file._
import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration
import scala.sys.process._

import munit.FunSuite

abstract class CommunitySuite extends FunSuite {

  import TestHelpers._

  override val munitTimeout = new duration.FiniteDuration(10, duration.MINUTES)

  protected def builds: Seq[CommunityBuild]

  for {
    build <- builds
    (k, v) <- TestStyles.styles
    if build.styles.isEmpty || build.styles.contains(k) == build.stylesIncluded
  } {
    val prefix = s"[ref ${build.commit}, style $k]"

    val style = build.fileOverrideConf.fold(v)(v.withFileOverride)
      .withDialect(NamedDialect(build.dialect))
    test(s"community-build: ${build.name} $prefix")(check(k, style)(build))
  }

  private def check(styleName: String, style: ScalafmtConfig)(implicit
      build: CommunityBuild,
  ): Unit = {
    val folder = fetchCommunityBuild
    val atomicStatesVisited = new AtomicInteger(0)
    implicit val customStyle: ScalafmtConfig = style
      .withCompleteCallback(x => atomicStatesVisited.getAndAdd(x.totalExplored))

    val stats = checkFilesRecursive(styleName, folder.toAbsolutePath)
      .getOrElse(TestStats.init)
    val timePer1KLines = Math
      .round(stats.timeTaken / (stats.linesParsed / 1000.0))
    val statesVisited = atomicStatesVisited.get()

    println("--------------------------")
    println(s"${build.name} [ref=${build.commit}] [style=$styleName]")
    println(s"Files parsed correctly ${stats.checkedFiles - stats.errors}")
    println(s"Files errored: ${stats.errors}")
    println(s"Total states visited: $statesVisited")
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
    if (!OsSpecific.isWindows) build.statsPerStyle.get(styleName).foreach { x =>
      assertEquals(statesVisited, x.expectedStatesVisited)
    }
  }

  private def fetchCommunityBuild(implicit build: CommunityBuild): Path = {
    try Files.createDirectories(communityProjectsDirectory)
    catch { case _: FileAlreadyExistsException => }

    val log = new ListBuffer[String]
    val logger = ProcessLogger(log += _)

    def runCmdRaw(cmd: String): Int = {
      log.clear()
      Console.err.println(s"Invoking command: $cmd")
      cmd.!(logger)
    }

    def runCmd(cmd: String, what: => String): Unit = {
      val result: Int = runCmdRaw(cmd)
      assertEquals(
        clue(result),
        0,
        log.mkString(
          s"Community build ${build.name}: $what failed:\n",
          "\n",
          "\n",
        ),
      )
    }

    val folderPath = communityProjectsDirectory.resolve(build.name)
    val folder = folderPath.toString

    if (!Files.exists(folderPath)) // clone
      if (OsSpecific.isWindows) {
        runCmd(s"git init $folder", "running init")
        runCmd(
          s"git -C $folder config core.longpaths true",
          "setting long paths",
        )
        runCmd(
          s"git -C $folder remote add origin ${build.giturl}",
          "adding origin",
        )
        runCmd(s"git -C $folder fetch --tags origin", s"fetching tags")
      } else runCmd(
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
