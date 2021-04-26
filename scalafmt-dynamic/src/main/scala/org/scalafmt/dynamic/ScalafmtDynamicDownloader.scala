package org.scalafmt.dynamic

import java.io.OutputStreamWriter
import java.net.URL

import coursierapi._

import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.util.Try

class ScalafmtDynamicDownloader(
    downloadProgressWriter: OutputStreamWriter,
    customRepositories: List[Repository],
    ttl: Option[Duration] = None
) {
  import ScalafmtDynamicDownloader._

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
      DownloadSuccess(version, urls)
    }.toEither.left.map {
      case e: error.ResolutionError =>
        DownloadResolutionError(e)
      case e =>
        DownloadUnknownError(e)
    }
  }

  private def dependencies(version: ScalafmtVersion): List[Dependency] =
    List(
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
    if (version < ScalafmtVersion(0, 7, 0, 0)) "2.11"
    else if (version < ScalafmtVersion(2, 1, 2, 0)) "2.12"
    else "2.13"

  @inline
  private def scalaVersion(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(0, 7, 0, 0)) "2.11.12"
    else if (version < ScalafmtVersion(2, 1, 2, 0)) BuildInfo.scala212
    else BuildInfo.scala

  @inline
  private def organization(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(2, 0, 0, 2)) {
      "com.geirsson"
    } else {
      "org.scalameta"
    }

  private def repositories: Array[Repository] = {
    // Default repositories are ivy2local, central and also anything in COURSIER_REPOSITORIES overrides
    customRepositories.toArray ++ Repository
      .defaults()
      .asScala ++ Array(
      MavenRepository.of(
        "https://oss.sonatype.org/content/repositories/snapshots"
      ),
      MavenRepository.of("https://oss.sonatype.org/content/repositories/public")
    )
  }

}

object ScalafmtDynamicDownloader {
  case class DownloadSuccess(version: ScalafmtVersion, jarUrls: Seq[URL])

  sealed trait DownloadFailure {
    def cause: Throwable
  }
  case class DownloadResolutionError(cause: error.ResolutionError)
      extends DownloadFailure
  case class DownloadUnknownError(cause: Throwable) extends DownloadFailure
}
