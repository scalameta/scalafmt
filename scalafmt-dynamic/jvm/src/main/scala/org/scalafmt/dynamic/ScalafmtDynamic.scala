package org.scalafmt.dynamic

import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.interfaces._

import java.nio.file.Path
import java.util.ServiceLoader

import scala.util.DynamicVariable

final case class ScalafmtDynamic(
    properties: ScalafmtProperties,
    _moduleLoader: ScalafmtModuleLoader,
    configLoader: ScalafmtConfigLoader,
) extends Scalafmt
    with RepositoryCredential.ScalafmtExtension
    with ScalafmtSessionFactory {

  val moduleLoader: ScalafmtModuleLoader = ScalafmtModuleLoader
    .CachedProxy(_moduleLoader)

  def this(
      moduleLoader: ScalafmtModuleLoader,
      reporter: ScalafmtReporter = ConsoleScalafmtReporter,
      configLoader: ScalafmtConfigLoader = ScalafmtConfigLoader,
  ) = this(
    properties = ScalafmtProperties(reporter = reporter),
    _moduleLoader = moduleLoader,
    configLoader = ScalafmtConfigLoader.CachedProxy(configLoader),
  )

  def this(dependencyDownloader: RepositoryPackageDownloaderFactory) =
    this(new ScalafmtModuleLoader.WithDownloader(dependencyDownloader))

  def this() = this(ScalafmtDynamic.defaultDependencyDownloader)

  override def clear(): Unit = moduleLoader.close()

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
      value: RepositoryCredential*,
  ): Scalafmt = copy(properties = properties.withRepositoryCredentials(value))

  override def format(config: Path, file: Path, code: String): String =
    createSession(config).format(file, code)

  override def createSession(config: Path): ScalafmtSession =
    resolveConfig(config).fold(
      error => { properties.reportError(config, error); throw error },
      ScalafmtDynamicSession(config, properties),
    )

  override def withRepositoryPackageDownloader(
      factory: RepositoryPackageDownloaderFactory,
  ): Scalafmt =
    copy(_moduleLoader = new ScalafmtModuleLoader.WithDownloader(factory))

  def resolveConfig(configPath: Path): FormatEval[ScalafmtReflectConfig] =
    configLoader.load(configPath, properties, moduleLoader)

}

private[dynamic] object ScalafmtDynamic {

  val defaultDependencyDownloaderOpt
      : DynamicVariable[Option[RepositoryPackageDownloaderFactory]] =
    new DynamicVariable(None)

  def defaultDependencyDownloader: RepositoryPackageDownloaderFactory =
    defaultDependencyDownloaderOpt.value.getOrElse(loadedDependencyDownloader)

  def defaultUncachedModuleLoader: ScalafmtModuleLoader =
    new ScalafmtModuleLoader.WithDownloader(defaultDependencyDownloader)

  def defaultUncachedConfigLoader: ScalafmtConfigLoader = ScalafmtConfigLoader

  private lazy val loadedDependencyDownloader
      : RepositoryPackageDownloaderFactory = {
    val factories = ServiceLoader
      .load(classOf[RepositoryPackageDownloaderFactory]).iterator()
    if (factories.hasNext) factories.next()
    else EmptyDependencyDownloaderFactory
  }

  private object EmptyDependencyDownloaderFactory
      extends RepositoryPackageDownloaderFactory {
    override def create(
        reporter: ScalafmtReporter,
        properties: RepositoryProperties,
    ): RepositoryPackageDownloader = throw new UnsupportedOperationException(
      """|Please register an implementation of
         |  `org.scalafmt.interfaces.RepositoryPackageDownloaderFactory`
         |as a service provider, or inject it directly into an instance of
         |  `org.scalafmt.interfaces.Scalafmt`
         |using the `withRepositoryPackageDownloader` method.
         |""".stripMargin,
    )
  }

}
