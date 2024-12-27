package org.scalafmt.dynamic

import java.io.OutputStreamWriter
import java.net.URI
import java.net.URL

import scala.collection.JavaConverters._
import scala.util.Try

import coursierapi.{Dependency => CoursierDependency, _}

private class CoursierDependencyDownloader(
    downloadProgressWriter: OutputStreamWriter,
    customRepositories: Seq[Repository],
) extends DependencyDownloader {

  override def download(dependencies: Seq[Dependency]): Try[Seq[URL]] = Try {
    val coursierDependencies = dependencies
      .map(x => CoursierDependency.of(x.group, x.artifact, x.version))
    // TODO: ttl is unavailable, see https://github.com/coursier/interface/issues/57
    val cache = Cache.create()
      .withLogger(Logger.progressBars(downloadProgressWriter))
    val settings = Fetch.create().withCache(cache)
      .withRepositories(repositories: _*)
      .withDependencies(coursierDependencies: _*)
    settings.fetch().asScala.map(_.toURI.toURL).toList
  }

  private def repositories: Seq[Repository] =
    // Default repositories are ivy2local, central and also anything in COURSIER_REPOSITORIES overrides
    customRepositories ++ Repository.defaults().asScala ++ Seq(
      "https://oss.sonatype.org/content/repositories/snapshots",
      "https://oss.sonatype.org/content/repositories/public",
    ).map(MavenRepository.of)

}

object CoursierDependencyDownloader extends DependencyDownloaderFactory {

  override def create(properties: ScalafmtProperties): DependencyDownloader = {
    val writer = properties.reporter.downloadOutputStreamWriter()
    val repositories = properties.repositories.map { x =>
      val host = new URI(x).getHost
      val repo = MavenRepository.of(x)
      properties.repositoryCredentials.find(_.host == host).fold(repo)(cred =>
        repo.withCredentials(Credentials.of(cred.username, cred.password)),
      )
    }
    new CoursierDependencyDownloader(writer, repositories)
  }

}
