package org.scalafmt.dynamic

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.dynamic.utils.ReentrantCache
import org.scalafmt.interfaces.RepositoryPackageDownloaderFactory
import org.scalafmt.sysops.PlatformRunOps

import java.io.Closeable
import java.net.URLClassLoader
import java.nio.file.Path

import scala.util.Try

trait ScalafmtModuleLoader {

  def load(
      configPath: Path,
      version: ScalafmtVersion,
      properties: ScalafmtProperties,
  ): FormatEval[ScalafmtReflect]

  def close(obj: ScalafmtReflect): Unit = obj.closeClassLoader(this)
}

object ScalafmtModuleLoader {

  class WithDownloader(factory: RepositoryPackageDownloaderFactory)
      extends ScalafmtModuleLoader {
    override def load(
        configPath: Path,
        version: ScalafmtVersion,
        properties: ScalafmtProperties,
    ): FormatEval[ScalafmtReflect] = {
      val (scalaVersion, dependencies) = Dependency.dependencies(version)
      Try(factory.create(properties.reporter, properties).download(
        scalaVersion,
        version.toString,
        properties.reporter,
        dependencies.asJava,
      )).fold(
        x => Left(new CannotDownload(configPath, version, x)),
        jars => {
          val urls = jars.asScala.map(_.toURI.toURL).toSeq
          Try {
            val classloader = new URLClassLoader(urls.toArray, null)
            ScalafmtReflect(classloader, version, ownedBy = this)
          }.toEither.left.map {
            case e: ReflectiveOperationException =>
              new CorruptedClassPath(configPath, version, urls, e)
            case e => new UnknownConfigError(configPath, e)
          }
        },
      )
    }
  }

  object CachedProxy {
    def apply(loader: ScalafmtModuleLoader): CachedProxy = loader match {
      case loader: CachedProxy => loader
      case _ => new CachedProxy(loader)
    }
  }

  class CachedProxy(loader: ScalafmtModuleLoader)
      extends ScalafmtModuleLoader with Closeable {
    private[dynamic] type Value = FormatEval[ScalafmtReflect]
    private[dynamic] val cache: ReentrantCache[ScalafmtVersion, Value] =
      ReentrantCache()

    override def load(
        configPath: Path,
        version: ScalafmtVersion,
        properties: ScalafmtProperties,
    ): FormatEval[ScalafmtReflect] = {
      def load() = loader.load(configPath, version, properties)
      cache.getOrAddToCache(version)(load)
    }

    override def close(obj: ScalafmtReflect): Unit = {}
    override def close(): Unit = {
      implicit val ec = PlatformRunOps.parasiticExecutionContext
      cache.clear().foreach(_.foreach(_.foreach(loader.close)))
    }

  }

}
