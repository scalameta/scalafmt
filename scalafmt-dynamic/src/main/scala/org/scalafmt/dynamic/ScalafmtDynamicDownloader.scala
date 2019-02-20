package org.scalafmt.dynamic

import java.io.PrintWriter
import java.net.URL
import java.nio.file.Path

import com.geirsson.coursiersmall._
import org.scalafmt.dynamic.ScalafmtDynamicDownloader._

import scala.concurrent.duration.Duration
import scala.util.Try

class ScalafmtDynamicDownloader(
    downloadProgressWriter: PrintWriter,
    ttl: Option[Duration] = None
) {

  def download(version: String): Either[DownloadFailure, DownloadSuccess] = {
    Try {
      val settings = new Settings()
        .withDependencies(dependencies(version))
        .withTtl(ttl.orElse(Some(Duration.Inf)))
        .withWriter(downloadProgressWriter)
        .withRepositories(repositories)
      val jars: Seq[Path] = CoursierSmall.fetch(settings)
      val urls = jars.map(_.toUri.toURL).toArray
      DownloadSuccess(version, urls)
    }.toEither.left.map {
      case e: ResolutionException =>
        DownloadResolutionError(version, e)
      case e =>
        DownloadUnknownError(version, e)
    }
  }

  private def dependencies(version: String): List[Dependency] = List(
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

  private def repositories: List[Repository] = List(
    Repository.MavenCentral,
    Repository.Ivy2Local,
    Repository.SonatypeReleases,
    Repository.SonatypeSnapshots
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
      cause: ResolutionException
  ) extends DownloadFailure
  case class DownloadUnknownError(version: String, cause: Throwable)
      extends DownloadFailure
}
