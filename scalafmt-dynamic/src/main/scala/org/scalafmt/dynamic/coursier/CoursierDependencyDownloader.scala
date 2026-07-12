package org.scalafmt.dynamic.coursier

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.interfaces._

import java.io.File
import java.{lang => jl}

import _root_.coursier._

class CoursierDependencyDownloader(customRepositories: Seq[MavenRepository])
    extends RepositoryPackageDownloader {

  override def download(
      scalaVersion: String,
      scalafmtVersion: String,
      reporter: ScalafmtReporter,
      dependencies: jl.Iterable[RepositoryPackage],
  ): jl.Iterable[File] = ClasspathCache
    .get(scalafmtVersion, scalaVersion, repositories).getOrElse {
      val downloadProgressWriter = reporter.downloadOutputStreamWriter()
      val files = Fetch(
        cache.FileCache() // this ctor preserves COURSIER_CREDENTIALS
          .withLogger(cache.loggers.RefreshLogger.create(downloadProgressWriter)),
      ).addDependencies(dependencies.asScala.map { dep =>
        val mod = Module(Organization(dep.group), ModuleName(dep.artifact))
        Dependency(mod, dep.version)
      }.toSeq: _*).addRepositories(repositories: _*).run()
      ClasspathCache.put(scalafmtVersion, scalaVersion, repositories, files)
      files
    }.asJava

  private def repositories = customRepositories ++
    CoursierDependencyDownloader.extraRepositories

}

object CoursierDependencyDownloader {

  private val extraRepositories = Seq(
    // central snapshots
    "https://central.sonatype.com/repository/maven-snapshots",
  ).map(MavenRepository.apply)

}
