package org.scalafmt.dynamic

import java.net.URLClassLoader
import java.nio.file.Path

import com.geirsson.coursiersmall._
import org.scalafmt.dynamic.ScalafmtDynamicDownloader._
import org.scalafmt.interfaces.ScalafmtReporter

import scala.concurrent.duration.Duration
import scala.util.Try
import scala.util.control.NonFatal

class ScalafmtDynamicDownloader(
    respectVersion: Boolean,
    respectExcludeFilters: Boolean,
    reporter: ScalafmtReporter,
    cacheConfig: Boolean,
    ttl: Option[Duration] = None
) {

  def download(config: Path, version: String): DownloadResult = {
    Try {
      val settings = new Settings()
        .withDependencies(dependencies(version))
        .withTtl(ttl.orElse(Some(Duration.Inf)))
        .withWriter(reporter.downloadWriter())
        .withRepositories(
          List(
            Repository.MavenCentral,
            Repository.Ivy2Local,
            Repository.SonatypeReleases,
            Repository.SonatypeSnapshots
          )
        )
      val jars: Seq[Path] = CoursierSmall.fetch(settings)
      val urls = jars.map(_.toUri.toURL).toArray
      val classloader = new URLClassLoader(urls, null)
      ScalafmtReflect(
        classloader,
        config,
        cacheConfig,
        version,
        respectVersion,
        respectExcludeFilters,
        reporter
      )
    }.toEither.left.map {
      case _: ResolutionException => DownloadFailure(version, None)
      case NonFatal(e) => DownloadFailure(version, Some(e))
    }
  }

  private def dependencies(version: String): List[Dependency] = {
    List(
      new Dependency(
        organization(version),
        s"scalafmt-cli_${scalaBinaryVersion(version)}",
        version
      ),
      new Dependency(
        "org.scala-lang",
        "scala-reflect",
        scalaVersion(version)
      )
    )
  }

  @inline
  private def scalaBinaryVersion(version: String): String =
    if (version.startsWith("0.")) "2.11"
    else "2.12"

  @inline
  private def scalaVersion(version: String): String =
    if (version.startsWith("0.")) BuildInfo.scala211
    else BuildInfo.scala

  @inline
  private def organization(version: String): String =
    if (version.startsWith("1") || version.startsWith("0") || version == "2.0.0-RC1") {
      "com.geirsson"
    } else {
      "org.scalameta"
    }

}

object ScalafmtDynamicDownloader {
  type DownloadResult = Either[DownloadFailure, ScalafmtReflect]

  case class DownloadFailure(version: String, cause: Option[Throwable])
}
