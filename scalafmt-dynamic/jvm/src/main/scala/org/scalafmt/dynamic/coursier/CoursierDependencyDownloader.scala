package org.scalafmt.dynamic.coursier

import org.scalafmt.dynamic.{Dependency => RepositoryPackage, _}

import java.io.{File, OutputStreamWriter}
import java.net.URL

import scala.util.Try

import _root_.coursier._

private class CoursierDependencyDownloader(
    downloadProgressWriter: OutputStreamWriter,
    customRepositories: Seq[MavenRepository],
) extends DependencyDownloader {

  override def download(dependencies: Seq[RepositoryPackage]): Try[Seq[URL]] =
    Try(downloadImpl(dependencies).map(_.toURI.toURL).toList)

  private def downloadImpl(dependencies: Seq[RepositoryPackage]): Seq[File] = {
    val fileCache = cache.FileCache() // this ctor preserves COURSIER_CREDENTIALS
      .withLogger(cache.loggers.RefreshLogger.create(downloadProgressWriter))
    Fetch(fileCache).addDependencies(dependencies.map { dep =>
      val mod = Module(Organization(dep.group), ModuleName(dep.artifact))
      Dependency(mod, dep.version)
    }: _*).addRepositories(repositories: _*).run()
  }

  private def repositories = customRepositories ++
    CoursierDependencyDownloader.extraRepositories

}

object CoursierDependencyDownloader {

  private val extraRepositories = Seq(
    // central snapshots
    "https://central.sonatype.com/repository/maven-snapshots",
  ).map(MavenRepository.apply)

}
