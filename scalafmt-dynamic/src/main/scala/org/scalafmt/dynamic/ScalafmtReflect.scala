package org.scalafmt.dynamic

import java.net.URLClassLoader
import java.nio.file.Path
import org.scalafmt.interfaces.ScalafmtReporter
import scala.util.Try

case class ScalafmtReflect(
    classLoader: URLClassLoader,
    configPath: Path,
    configStr: String,
    version: String,
    respectVersion: Boolean,
    respectProjectFilters: Boolean,
    reporter: ScalafmtReporter
) {
  private val formatted = classLoader.loadClass("org.scalafmt.Formatted")
  private val parseException =
    classLoader.loadClass("scala.meta.parsers.ParseException")
  private val tokenizeException =
    classLoader.loadClass("scala.meta.tokenizers.TokenizeException")
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

  def parseConfig(): Object = {
    val configured: Object = try { // scalafmt >= 1.6.0
      val parseHoconConfig =
        scalafmtCls.getMethod("parseHoconConfig", classOf[String])
      parseHoconConfig.invoke(null, configStr)
    } catch {
      case _: NoSuchMethodException =>
        // scalafmt >= v0.7.0-RC1 && scalafmt < 1.6.0
        val fromHocon =
          configCls.getMethod("fromHoconString", classOf[String], optionCls)
        val fromHoconEmptyPath =
          configCls.getMethod("fromHoconString$default$2").invoke(null)
        fromHocon.invoke(null, configStr, fromHoconEmptyPath)
    }
    try invoke(configured, "get")
    catch {
      case ReflectionException(e) =>
        throw ScalafmtConfigException(e.getMessage)
    }
  }

  def format(filename: Path, code: String): String = {
    if (config == null) {
      config = parseConfig()
      reporter.parsedConfig(configPath, version)
    }
    formatInternal(filename, code, config)
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
      try {
        formattedGet.invoke(formatted).asInstanceOf[String]
      } catch {
        case ReflectionException(e)
            if tokenizeException.isInstance(e) ||
              parseException.isInstance(e) =>
          val pos = invoke(e, "pos")
          val range = positionRange(pos)
          val shortMessage = invokeAs[String](e, "shortMessage")
          throw PositionExceptionImpl(
            file,
            code,
            shortMessage,
            e.getMessage,
            range,
            e
          )
      }
    }
  }

  private def positionRange(pos: Object): RangePosition = {
    try {
      RangePosition(
        invokeAs[Int](pos, "startLine"),
        invokeAs[Int](pos, "startColumn"),
        invokeAs[Int](pos, "endLine"),
        invokeAs[Int](pos, "endColumn")
      )
    } catch {
      case _: NoSuchMethodException =>
        val start = invoke(pos, "start")
        val end = invoke(pos, "end")
        RangePosition(
          invokeAs[Int](start, "line"),
          invokeAs[Int](start, "column"),
          invokeAs[Int](end, "line"),
          invokeAs[Int](end, "column")
        )
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
    if (!respectProjectFilters) false
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

  private def invokeAs[T](
      obj: Object,
      toInvoke: String,
      args: (Class[_], Object)*
  ): T = {
    invoke(obj, toInvoke, args: _*).asInstanceOf[T]
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
