package org.scalafmt.dynamic

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}

import org.scalafmt.interfaces.ScalafmtReporter

import scala.reflect.ClassTag
import scala.util.Try

case class ScalafmtReflect(
    classLoader: URLClassLoader,
    configFile: Path,
    version: String,
    respectVersion: Boolean,
    respectProjectFilters: Boolean,
    reporter: ScalafmtReporter
) {
  import classLoader.loadClass
  import org.scalafmt.dynamic.ScalafmtReflect._

  private val formattedCls = loadClass("org.scalafmt.Formatted")
  private val scalaSetCls = loadClass("scala.collection.immutable.Set")
  private val optionCls = loadClass("scala.Option")
  private val configCls = loadClass("org.scalafmt.config.Config")
  private val scalafmtCls = loadClass("org.scalafmt.Scalafmt")
  private val dialectCls = loadClass("scala.meta.Dialect")
  private val dialectsCls = loadClass("scala.meta.dialects.package")

  private val parseExceptionCls =
    loadClass("scala.meta.parsers.ParseException")
  private val tokenizeExceptionCls =
    loadClass("scala.meta.tokenizers.TokenizeException")

  private val defaultScalaFmtConfig =
    scalafmtCls.invokeStatic("format$default$2")
  private val emptyRange =
    scalafmtCls.invokeStatic("format$default$3")

  private val formattedGet = formattedCls.getMethod("get")
  private val formatMethod = scalafmtCls.getMethod(
    "format",
    classOf[String],
    defaultScalaFmtConfig.getClass,
    scalaSetCls
  )
  private val formatMethodWithFilename = Try(
    scalafmtCls.getMethod(
      "format",
      classOf[String],
      defaultScalaFmtConfig.getClass,
      scalaSetCls,
      classOf[String]
    )
  ).toOption

  private val sbtDialect: Object = {
    try dialectsCls.invokeStatic("Sbt")
    catch {
      case ReflectionException(_: NoSuchMethodException) =>
        dialectsCls.invokeStatic("Sbt0137")
    }
  }
  private var config: Object = _

  def readConfig(): String = {
    new String(Files.readAllBytes(configFile), StandardCharsets.UTF_8)
  }

  def parseConfig(): Object = {
    val configText = readConfig()
    val configured: Object = try { // scalafmt >= 1.6.0
      scalafmtCls.invokeStatic("parseHoconConfig", configText.asParam)
    } catch {
      case _: NoSuchMethodException =>
        // scalafmt >= v0.7.0-RC1 && scalafmt < 1.6.0
        val fromHoconEmptyPath =
          configCls.invokeStatic("fromHoconString$default$2")
        configCls.invokeStatic(
          "fromHoconString",
          (classOf[String], configText),
          (optionCls, fromHoconEmptyPath))
    }
    try configured.invoke("get")
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
          config.invoke("withDialect", (dialectCls, sbtDialect))
        } else {
          config
        }
      val formatted = formatMethodWithFilename match {
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
            if tokenizeExceptionCls.isInstance(e) ||
              parseExceptionCls.isInstance(e) =>
          val pos = e.invoke("pos")
          val range = positionRange(pos)
          val shortMessage = e.invokeAs[String]("shortMessage")
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
        pos.invokeAs[Int]("startLine"),
        pos.invokeAs[Int]("startColumn"),
        pos.invokeAs[Int]("endLine"),
        pos.invokeAs[Int]("endColumn")
      )
    } catch {
      case _: NoSuchMethodException =>
        val start = pos.invoke("start")
        val end = pos.invoke("end")
        RangePosition(
          start.invokeAs[Int]("line"),
          start.invokeAs[Int]("column"),
          end.invokeAs[Int]("line"),
          end.invokeAs[Int]("column")
        )
    }
  }

  private def clearTokenizerCache(): Unit = {
    val cache = moduleInstance(
      "scala.meta.internal.tokenizers.PlatformTokenizerCache$"
    )
    cache.invoke("megaCache").invoke("clear")
  }

  private def checkVersionMismatch(config: Object): Unit = {
    if (respectVersion) {
      val obtained = config.invokeAs[String]("version")
      if (obtained != version) {
        throw VersionMismatch(obtained, version)
      }
    }
  }

  private def isIgnoredFile(filename: String, config: Object): Boolean = {
    if (!respectProjectFilters) false
    else {
      val matcher = config.invoke("project").invoke("matcher")
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
}

object ScalafmtReflect {

  private implicit class ObjectReflectOps[T](val target: T) extends AnyVal {
    def invokeAs[R](methodName: String, args: (Class[_], Object)*): R = {
      invoke(methodName, args: _*).asInstanceOf[R]
    }

    def invoke(methodName: String, args: (Class[_], Object)*): Object = {
      val clazz = target.getClass
      val method = clazz.getMethod(methodName, args.map(_._1): _*)
      method.invoke(target, args.map(_._2): _*)
    }

    def asParam(implicit classTag: ClassTag[T]): (Class[_], T) = {
      (classTag.runtimeClass, target)
    }
  }

  private implicit class ClassReflectOps(val clazz: Class[_]) extends AnyVal {
    def invokeStaticAs[T](methodName: String, args: (Class[_], Object)*): T = {
      invokeStatic(methodName, args: _*).asInstanceOf[T]
    }

    def invokeStatic(methodName: String, args: (Class[_], Object)*): Object = {
      val method = clazz.getMethod(methodName, args.map(_._1): _*)
      method.invoke(null, args.map(_._2): _*)
    }
  }

}
