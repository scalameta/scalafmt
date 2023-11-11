package org.scalafmt.dynamic

import java.nio.file.Path

import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.interfaces._

final case class ScalafmtDynamic(
    properties: ScalafmtProperties,
    moduleLoader: ScalafmtModuleLoader,
    configLoader: ScalafmtConfigLoader
) extends Scalafmt
    with RepositoryCredential.ScalafmtExtension
    with ScalafmtSessionFactory {

  def this() = this(
    ScalafmtProperties(),
    new ScalafmtModuleLoader.CachedProxy(
      ScalafmtDynamic.defaultUncachedModuleLoader
    ),
    new ScalafmtConfigLoader.CachedProxy(
      ScalafmtDynamic.defaultUncachedConfigLoader
    )
  )

  override def clear(): Unit = {
    moduleLoader.close()
  }

  override def withReporter(value: ScalafmtReporter): ScalafmtDynamic =
    copy(properties = properties.withReporter(value))

  override def withRespectProjectFilters(value: Boolean): ScalafmtDynamic =
    copy(properties = properties.withRespectProjectFilters(value))

  override def withRespectVersion(respectVersion: Boolean): ScalafmtDynamic =
    if (respectVersion) this
    else throw new ScalafmtInterfaceMethodDeprecated("withRespectVersion")

  override def withDefaultVersion(defaultVersion: String): ScalafmtDynamic =
    throw new ScalafmtInterfaceMethodDeprecated("withDefaultVersion")

  override def withMavenRepositories(value: String*): Scalafmt =
    copy(properties = properties.withMavenRepositories(value))

  override def withRepositoryCredentials(
      value: RepositoryCredential*
  ): Scalafmt =
    copy(properties = properties.withRepositoryCredentials(value))

  override def format(config: Path, file: Path, code: String): String =
    createSession(config).format(file, code)

  override def createSession(config: Path): ScalafmtSession =
    resolveConfig(config).fold(
      error => { properties.reportError(config, error); throw error },
      new ScalafmtDynamicSession(properties, _)
    )

  def resolveConfig(configPath: Path): FormatEval[ScalafmtReflectConfig] =
    configLoader.load(configPath, properties, moduleLoader)

}

private[dynamic] object ScalafmtDynamic {

  def defaultUncachedModuleLoader =
    new ScalafmtModuleLoader.WithDownloader(CoursierDependencyDownloader)

  def defaultUncachedConfigLoader = ScalafmtConfigLoader

}
