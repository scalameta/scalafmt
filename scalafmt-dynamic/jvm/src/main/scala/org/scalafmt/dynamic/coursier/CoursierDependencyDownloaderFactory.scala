package org.scalafmt.dynamic.coursier

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.interfaces._

import java.net.URI

import _root_.coursier._

class CoursierDependencyDownloaderFactory
    extends RepositoryPackageDownloaderFactory {

  override def create(
      reporter: ScalafmtReporter,
      props: RepositoryProperties,
  ): RepositoryPackageDownloader = {
    val repositories = props.getRepositories.asScala.map { x =>
      val host = new URI(x).getHost
      val repo = MavenRepository(x)
      props.getCredentials.asScala.find(_.host == host).fold(repo) { cred =>
        val auth = core.Authentication(cred.username, cred.password)
        repo.withAuthentication(Some(auth))
      }
    }.toList
    new CoursierDependencyDownloader(repositories)
  }

}

object CoursierDependencyDownloaderFactory
    extends CoursierDependencyDownloaderFactory
