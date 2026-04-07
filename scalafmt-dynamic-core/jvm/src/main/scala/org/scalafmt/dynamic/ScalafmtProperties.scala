package org.scalafmt.dynamic

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.interfaces._

import java.nio.file.Path
import java.{lang => jl}

final case class ScalafmtProperties(
    reporter: ScalafmtReporter = ConsoleScalafmtReporter,
    repositories: Seq[String] = Nil,
    repositoryCredentials: Seq[RepositoryCredential] = Nil,
    respectExcludeFilters: Boolean = true,
) extends RepositoryProperties {

  def withReporter(value: ScalafmtReporter): ScalafmtProperties =
    copy(reporter = value)

  def withRespectProjectFilters(value: Boolean): ScalafmtProperties =
    copy(respectExcludeFilters = value)

  def withMavenRepositories(value: Seq[String]): ScalafmtProperties =
    copy(repositories = value)

  def withRepositoryCredentials(
      value: Seq[RepositoryCredential],
  ): ScalafmtProperties = copy(repositoryCredentials = value)

  def reportError(file: Path, error: ScalafmtDynamicError): Unit = error match {
    case _: ConfigMissingVersion => reporter
        .missingVersion(file, BuildInfo.stable)
    case _ => reporter.error(file, error.getMessage, error.getCause)
  }

  override def getRepositories: jl.Iterable[String] = repositories.asJava
  override def getCredentials: jl.Iterable[RepositoryCredential] =
    repositoryCredentials.asJava

}
