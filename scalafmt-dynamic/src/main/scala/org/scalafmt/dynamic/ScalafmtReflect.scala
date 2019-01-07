package org.scalafmt.dynamic

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.FileTime
import org.scalafmt.interfaces.ScalafmtReporter
import scala.util.Try

case class ScalafmtReflect(
    classLoader: URLClassLoader,
    configFile: Path,
    version: String,
    respectVersion: Boolean,
    respectProjectFilters: Boolean,
    reporter: ScalafmtReporter
) {
  private val formatted = classLoader.loadClass("org.scalafmt.Formatted")
  private val formattedGet = formatted.getMethod("get")
  private val scalafmt = classLoader.loadClass("org.scalafmt.Scalafmt")
  private val scalaSet = classLoader.loadClass("scala.collection.immutable.Set")
  private val defaultScalaFmtConfig =
    scalafmt.getMethod("format$default$2").invoke(null)
  private val emptyRange = scalafmt.getMethod("format$default$3").invoke(null)
  private val formatMethod = scalafmt.getMethod(
    "format",
    classOf[String],
    defaultScalaFmtConfig.getClass,
    scalaSet
  )
  private val formatFilenameMethod = Try(
    scalafmt.getMethod(
      "format",
      classOf[String],
      defaultScalaFmtConfig.getClass,
      scalaSet,
      classOf[String]
    )
  ).toOption
  private val optionCls = classLoader.loadClass("scala.Option")
  private val configCls = classLoader.loadClass("org.scalafmt.config.Config")
  private val scalafmtCls = classLoader.loadClass("org.scalafmt.Scalafmt")
  private val dialectCls = classLoader.loadClass("scala.meta.Dialect")
  private val dialectsCls = classLoader.loadClass("scala.meta.dialects.package")
  private val sbtDialect: Object = {
    try dialectsCls.getMethod("Sbt").invoke(null)
    catch {
      case ReflectionException(_: NoSuchMethodException) =>
        dialectsCls.getMethod("Sbt0137").invoke(null)
    }
  }
  private var config: Object = _

  def readConfig(): String = {
    new String(Files.readAllBytes(configFile), StandardCharsets.UTF_8)
  }

  def parseConfig(): Object = {
    val configText = readConfig()
    val configured: Object = try { // scalafmt >= 1.6.0
      val parseHoconConfig =
        scalafmtCls.getMethod("parseHoconConfig", classOf[String])
      parseHoconConfig.invoke(null, configText)
    } catch {
      case _: NoSuchMethodException =>
        // scalafmt >= v0.7.0-RC1 && scalafmt < 1.6.0
        val fromHocon =
          configCls.getMethod("fromHoconString", classOf[String], optionCls)
        val fromHoconEmptyPath =
          configCls.getMethod("fromHoconString$default$2").invoke(null)
        fromHocon.invoke(null, configText, fromHoconEmptyPath)
    }
    try invoke(configured, "get")
    catch {
      case ReflectionException(e) =>
        throw ScalafmtConfigException(e.getMessage)
    }
  }

  private var lastTimestamp = FileTime.fromMillis(0)

  def format(filename: Path, code: String): String = {
    val currentTimestamp = Files.getLastModifiedTime(configFile)
    if (currentTimestamp.compareTo(lastTimestamp) != 0) {
      config = parseConfig()
      lastTimestamp = currentTimestamp
      reporter.parsedConfig(configFile, version)
      formatInternal(filename, code, config)
    } else {
      formatInternal(filename, code, config)
    }
  }

  private def formatInternal(
      file: Path,
      code: String,
      config: Object
  ): String = {
    checkVersionMismatch(config)
    val filename = file.toString
    if (isIgnoredFile(filename, config)) {
      reporter.excluded(file)
      code
    } else {
      val dialectConfig =
        if (filename.endsWith(".sbt") || filename.endsWith(".sc")) {
          invoke(config, "withDialect", (dialectCls, sbtDialect))
        } else {
          config
        }
      val formatted = formatFilenameMethod match {
        case Some(method) =>
          method.invoke(null, code, dialectConfig, emptyRange, filename)
        case None =>
          formatMethod.invoke(null, code, dialectConfig, emptyRange)
      }
      clearTokenizerCache()
      formattedGet.invoke(formatted).asInstanceOf[String]
    }
  }

  private def clearTokenizerCache(): Unit = {
    val cache = moduleInstance(
      "scala.meta.internal.tokenizers.PlatformTokenizerCache$"
    )
    invoke(invoke(cache, "megaCache"), "clear")
  }

  private def checkVersionMismatch(config: Object): Unit = {
    if (respectVersion) {
      val obtained = invoke(config, "version").asInstanceOf[String]
      if (obtained != version) {
        throw VersionMismatch(obtained, version)
      }
    }
  }

  private def isIgnoredFile(filename: String, config: Object): Boolean = {
    if (!respectProjectFilters) true
    else {
      val matcher = invoke(invoke(config, "project"), "matcher")
      val matches = matcher.getClass.getMethod("matches", classOf[String])
      !matches.invoke(matcher, filename).asInstanceOf[java.lang.Boolean]
    }
  }

  private def moduleInstance(fqn: String): Object = {
    val cls = classLoader.loadClass(fqn)
    val module = cls.getField("MODULE$")
    module.setAccessible(true)
    module.get(null)
  }

  private def invoke(
      obj: Object,
      toInvoke: String,
      args: (Class[_], Object)*
  ): Object = {
    val clazz = obj.getClass
    val method = clazz.getMethod(toInvoke, args.map(_._1): _*)
    method.invoke(obj, args.map(_._2): _*)
  }
}
