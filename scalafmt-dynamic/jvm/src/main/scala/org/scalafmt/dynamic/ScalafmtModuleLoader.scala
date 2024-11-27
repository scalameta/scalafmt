package org.scalafmt.dynamic

import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.dynamic.utils.ReentrantCache

import java.io.Closeable
import java.net.URL
import java.net.URLClassLoader
import java.nio.file.Path

import scala.concurrent.ExecutionContext
import scala.util.Try

trait ScalafmtModuleLoader extends Closeable {

  def load(
      configPath: Path,
      version: ScalafmtVersion,
      properties: ScalafmtProperties,
  ): FormatEval[ScalafmtReflect]

}

object ScalafmtModuleLoader {

  class WithDownloader(downloader: DependencyDownloaderFactory)
      extends ScalafmtModuleLoader {
    override def load(
        configPath: Path,
        version: ScalafmtVersion,
        properties: ScalafmtProperties,
    ): FormatEval[ScalafmtReflect] = {
      val dependencies = Dependency.dependencies(version)
      val urls = downloader.create(properties).download(dependencies)
      urls.fold(
        x => Left(new CannotDownload(configPath, version, x)),
        loadClassPath(configPath, version),
      )
    }

    override def close(): Unit = {}
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

    override def close(): Unit = {
      cache.clear()
        .foreach(_.foreach(_.right.foreach(_.close()))(ExecutionContext.global))
      loader.close()
    }
  }

  private def loadClassPath(configPath: Path, version: ScalafmtVersion)(
      urls: Seq[URL],
  ): FormatEval[ScalafmtReflect] = Try {
    val classloader = new URLClassLoader(urls.toArray, null)
    ScalafmtReflect(classloader, version)
  }.toEither.left.map {
    case e: ReflectiveOperationException =>
      new CorruptedClassPath(configPath, version, urls, e)
    case e => new UnknownConfigError(configPath, e)
  }

}
