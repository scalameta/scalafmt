package org.scalafmt.dynamic

import java.io.PrintWriter
import java.net.URL
import java.nio.file.Path

import coursierapi._

import scala.collection.JavaConverters._
import org.scalafmt.dynamic.ScalafmtDynamicDownloader._

import scala.concurrent.duration.Duration
import scala.util.Try
import java.io.OutputStream
import java.io.PrintStream
import java.io.OutputStreamWriter
import scala.concurrent.JavaConversions._

class ScalafmtDynamicDownloader(
    downloadProgressWriter: OutputStreamWriter,
    ttl: Option[Duration] = None
) {

  def download(version: String): Either[DownloadFailure, DownloadSuccess] = {
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
        DownloadResolutionError(version, e)
      case e =>
        DownloadUnknownError(version, e)
    }
  }

  private def dependencies(version: String): List[Dependency] = List(
    Dependency.of(
      organization(version),
      s"scalafmt-cli_${scalaBinaryVersion(version)}",
      version
    ),
    Dependency.of(
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

  private def repositories: Array[Repository] = {
    // Default repositories are ivy2local, central and also anything in COURSIER_REPOSITORIES overrides
    Repository.defaults().asScala.toArray ++ Array(
      MavenRepository.of(
        "https://oss.sonatype.org/content/repositories/snapshots"
      ),
      MavenRepository.of("https://oss.sonatype.org/content/repositories/public")
    )
  }

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
}
