package org.scalafmt.dynamic

import java.io.PrintWriter
import java.net.URL
import java.nio.file.Path

import coursierapi._
import scala.collection.JavaConverters._
import org.scalafmt.dynamic.ScalafmtDynamicDownloader._
import org.scalafmt.dynamic.ScalafmtVersion
import org.scalafmt.dynamic.ScalafmtVersion.InvalidVersionException

import scala.concurrent.duration.Duration
import scala.util.Try
import java.io.OutputStream
import java.io.PrintStream
import java.io.OutputStreamWriter

class ScalafmtDynamicDownloader(
    downloadProgressWriter: OutputStreamWriter,
    ttl: Option[Duration] = None
) {

  def download(version: String): Either[DownloadFailure, DownloadSuccess] =
    ScalafmtVersion
      .parse(version)
      .left
      .map(InvalidVersionError(version, _))
      .flatMap(download)

  def download(
      version: ScalafmtVersion
  ): Either[DownloadFailure, DownloadSuccess] = {
    Try {
      val settings = Fetch
        .create()
        .withCache(
          Cache
            .create()
            // TODO: set ttl to infinity, see https://github.com/coursier/interface/issues/57
            // .withTtl(ttl.orElse(Some(Duration.Inf)))
            .withLogger(Logger.progressBars(downloadProgressWriter))
        )
        .withDependencies(dependencies(version).toArray: _*)
        .withRepositories(repositories: _*)
        .withCache(Cache.create())
      val urls: Array[URL] =
        settings.fetch().asScala.iterator.map(_.toURI.toURL).toArray
      DownloadSuccess(version.toString, urls)
    }.toEither.left.map {
      case e: error.ResolutionError =>
        DownloadResolutionError(version.toString, e)
      case e =>
        DownloadUnknownError(version.toString, e)
    }
  }

  private def dependencies(version: ScalafmtVersion): List[Dependency] = List(
    Dependency.of(
      organization(version),
      s"scalafmt-cli_${scalaBinaryVersion(version)}",
      version.toString
    ),
    Dependency.of(
      "org.scala-lang",
      "scala-reflect",
      scalaVersion(version)
    )
  )

  @inline
  private def scalaBinaryVersion(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(1, 0, 0, 0)) "2.11"
    else if (version < ScalafmtVersion(2, 2, 0, 0)) "2.12"
    else "2.13"

  @inline
  private def scalaVersion(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(1, 0, 0, 0)) BuildInfo.scala211
    else if (version < ScalafmtVersion(2, 2, 0, 0)) BuildInfo.scala212
    else BuildInfo.scala

  @inline
  private def organization(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(2, 0, 0, 2)) {
      "com.geirsson"
    } else {
      "org.scalameta"
    }

  private def repositories: Array[Repository] = Array(
    Repository.central(),
    Repository.ivy2Local(),
    MavenRepository.of(
      "https://oss.sonatype.org/content/repositories/snapshots"
    ),
    MavenRepository.of("https://oss.sonatype.org/content/repositories/public")
  )
}

object ScalafmtDynamicDownloader {
  sealed trait DownloadResult {
    def version: String
  }
  case class DownloadSuccess(version: String, jarUrls: Seq[URL])
      extends DownloadResult
  sealed trait DownloadFailure extends DownloadResult {
    def cause: Throwable
  }
  case class DownloadResolutionError(
      version: String,
      cause: error.ResolutionError
  ) extends DownloadFailure
  case class DownloadUnknownError(version: String, cause: Throwable)
      extends DownloadFailure
  case class InvalidVersionError(
      version: String,
      cause: InvalidVersionException
  ) extends DownloadFailure
}
