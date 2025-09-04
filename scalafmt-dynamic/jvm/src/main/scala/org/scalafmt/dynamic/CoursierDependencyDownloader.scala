package org.scalafmt.dynamic

import org.scalafmt.CompatCollections.JavaConverters._

import java.io.OutputStreamWriter
import java.net.URI
import java.net.URL

import scala.util.Try

import coursierapi.{Dependency => CoursierDependency, _}

private class CoursierDependencyDownloader(
    downloadProgressWriter: OutputStreamWriter,
    repositories: Seq[Repository],
) extends DependencyDownloader {

  override def download(dependencies: Seq[Dependency]): Try[Seq[URL]] = Try {
    val coursierDependencies = dependencies
      .map(x => CoursierDependency.of(x.group, x.artifact, x.version))
    // TODO: ttl is unavailable, see https://github.com/coursier/interface/issues/57
    val cache = Cache.create()
      .withLogger(Logger.progressBars(downloadProgressWriter))
    val settings = Fetch.create().withCache(cache)
      .addRepositories(repositories: _*)
      .withDependencies(coursierDependencies: _*)
    settings.fetch().asScala.map(_.toURI.toURL).toList
  }

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
