package org.scalafmt.dynamic.coursier

import org.scalafmt.dynamic._

import java.net.URI

import _root_.coursier._

object CoursierDependencyDownloaderFactory extends DependencyDownloaderFactory {

  override def create(properties: ScalafmtProperties): DependencyDownloader = {
    val writer = properties.reporter.downloadOutputStreamWriter()
    val repositories = properties.repositories.map { x =>
      val host = new URI(x).getHost
      val repo = MavenRepository(x)
      properties.repositoryCredentials.find(_.host == host).fold(repo) { cred =>
        val auth = core.Authentication(cred.username, cred.password)
        repo.withAuthentication(Some(auth))
      }
    }
    new CoursierDependencyDownloader(writer, repositories)
  }

}
