package org.scalafmt.dynamic

import java.io.OutputStreamWriter
import java.net.{URI, URL}

import scala.util.Try

import coursier.{Dependency => _, MavenRepository => Repository, _}

private class CoursierDependencyDownloader(
    downloadProgressWriter: OutputStreamWriter,
    repositories: Seq[Repository],
) extends DependencyDownloader {

  override def download(dependencies: Seq[Dependency]): Try[Seq[URL]] = Try {
    val fileCache = cache.FileCache() // this ctor preserves COURSIER_CREDENTIALS
      .withLogger(cache.loggers.RefreshLogger.create(downloadProgressWriter))
    Fetch(fileCache).addDependencies(dependencies.map { dep =>
      val mod = Module(Organization(dep.group), ModuleName(dep.artifact))
      core.Dependency(mod, dep.version)
    }: _*).addRepositories(repositories: _*).run().map(_.toURI.toURL).toList
  }

}

object CoursierDependencyDownloader extends DependencyDownloaderFactory {

  override def create(properties: ScalafmtProperties): DependencyDownloader = {
    val writer = properties.reporter.downloadOutputStreamWriter()
    val repositories = properties.repositories.map { x =>
      val host = new URI(x).getHost
      val repo = Repository(x)
      properties.repositoryCredentials.find(_.host == host).fold(repo)(cred =>
        repo.withAuthentication(Some(
          core.Authentication(cred.username, cred.password),
        )),
      )
    }
    new CoursierDependencyDownloader(writer, repositories)
  }

}
