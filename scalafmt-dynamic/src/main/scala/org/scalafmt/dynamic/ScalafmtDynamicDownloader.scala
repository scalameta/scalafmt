package org.scalafmt.dynamic

import java.io.{File, PrintWriter}
import java.net.URL

import org.scalafmt.dynamic.ScalafmtDynamicDownloader._

import coursier._
import coursier.error.ResolutionError
import coursier.cache.FileCache
import coursier.cache.loggers.RefreshLogger

import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

class ScalafmtDynamicDownloader(
    downloadProgressWriter: PrintWriter,
    ttl: Option[Duration] = None
) {

  def download(version: String): Either[DownloadFailure, DownloadSuccess] = {
    try {
      val jars: Seq[File] = Fetch()
        .addDependencies(dependencies(version): _*)
        .addRepositories(repositories: _*)
        .withResolveCache(
          FileCache().noCredentials
            .withTtl(ttl.orElse(Some(Duration.Inf)))
            .withLogger(
              new RefreshLogger(
                downloadProgressWriter,
                RefreshLogger.defaultDisplay(),
                fallbackMode = true
              )
            )
        )
        .run()
      val urls = jars.map(_.toPath.toUri.toURL).toArray
      Right(DownloadSuccess(version, urls))
    } catch {
      case e: ResolutionError =>
        Left(DownloadResolutionError(version, e))
      case e if NonFatal(e) =>
        Left(DownloadUnknownError(version, e))
    }
  }

  private def dependencies(version: String): List[Dependency] = List(
    Dependency.of(
      Module(
        organization(version),
        ModuleName(s"scalafmt-cli_${scalaBinaryVersion(version)}")
      ),
      version
    ),
    Dependency.of(
      Module(org"org.scala-lang", name"scala-reflect"),
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
  private def organization(version: String): Organization =
    if (version.startsWith("1") || version.startsWith("0") || version == "2.0.0-RC1") {
      org"com.geirsson"
    } else {
      org"org.scalameta"
    }

  private def repositories: List[Repository] = List(
    Repositories.central,
    LocalRepositories.ivy2Local,
    Repositories.sonatype("releases"),
    Repositories.sonatype("snapshots")
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
      cause: ResolutionError
  ) extends DownloadFailure
  case class DownloadUnknownError(version: String, cause: Throwable)
      extends DownloadFailure
}
