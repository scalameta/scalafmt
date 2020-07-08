package org.scalafmt.dynamic

import com.typesafe.config.ConfigFactory
import java.net.URLClassLoader
import java.nio.file.Path
import org.scalafmt.dynamic.exceptions._
import org.scalafmt.dynamic.utils.ReflectUtils._
import scala.util.Try

case class ScalafmtReflect(
    classLoader: URLClassLoader,
    version: ScalafmtVersion,
    respectVersion: Boolean
) {
  import classLoader.loadClass

  // FIXME: the class does not exist for version old versions, e.g. v0.2.8
  private val formattedCls = loadClass("org.scalafmt.Formatted")
  private val scalaSetCls = loadClass("scala.collection.immutable.Set")
  private val optionCls = loadClass("scala.Option")
  private val configCls = loadClass("org.scalafmt.config.Config")
  private val scalafmtCls = loadClass("org.scalafmt.Scalafmt")

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

  // TODO: see implementation details for other versions of scalafmt, find where intellij config is kept
  lazy val intellijScalaFmtConfig: Option[ScalafmtReflectConfig] = {
    if (version == ScalafmtVersion(1, 5, 1)) {
      val scalaFmtConfigCls =
        classLoader.loadClass("org.scalafmt.config.ScalafmtConfig")
      val configTarget = scalaFmtConfigCls.invokeStatic("intellij")
      Some(new ScalafmtReflectConfig(this, configTarget, classLoader))
    } else {
      None
    }
  }

  def parseConfig(configPath: Path): ScalafmtReflectConfig = {
    val configText = ConfigFactory.parseFile(configPath.toFile).root.render()
    val configured: Object =
      try { // scalafmt >= 1.6.0
        scalafmtCls.invokeStatic("parseHoconConfig", configText.asParam)
      } catch {
        case _: NoSuchMethodException =>
          // scalafmt >= v0.7.0-RC1 && scalafmt < 1.6.0
          val fromHoconEmptyPath =
            configCls.invokeStatic("fromHoconString$default$2")
          configCls.invokeStatic(
            "fromHoconString",
            configText.asParam,
            fromHoconEmptyPath.asParam(optionCls)
          )
        case ReflectionException(e) =>
          throw new ScalafmtDynamicError.ConfigParseError(
            configPath,
            e.getMessage
          )
      }

    try {
      new ScalafmtReflectConfig(this, configured.invoke("get"), classLoader)
    } catch {
      case ReflectionException(e) =>
        throw new ScalafmtDynamicError.ConfigParseError(
          configPath,
          e.getMessage
        )
    }
  }

  def format(
      code: String,
      config: ScalafmtReflectConfig,
      fileOpt: Option[Path] = None
  ): String = {
    require(this eq config.fmtReflect)
    checkVersionMismatch(config)

    val formatted = (formatMethodWithFilename, fileOpt) match {
      case (Some(method), Some(file)) =>
        val filename = file.toString
        method.invoke(null, code, config.target, emptyRange, filename)
      case _ =>
        formatMethod.invoke(null, code, config.target, emptyRange)
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
          fileOpt.orNull,
          code,
          shortMessage,
          e.getMessage,
          range,
          e
        )
    }
  }

  private def positionRange(pos: Object): RangePosition = {
    try {
      RangePosition(
        pos.invokeAs[Int]("start"),
        pos.invokeAs[Int]("startLine"),
        pos.invokeAs[Int]("startColumn"),
        pos.invokeAs[Int]("end"),
        pos.invokeAs[Int]("endLine"),
        pos.invokeAs[Int]("endColumn")
      )
    } catch {
      case _: ReflectiveOperationException | _: ClassCastException =>
        val start = pos.invoke("start")
        val end = pos.invoke("end")
        RangePosition(
          start.invokeAs[Int]("offset"),
          start.invokeAs[Int]("line"),
          start.invokeAs[Int]("column"),
          end.invokeAs[Int]("offset"),
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

  private def checkVersionMismatch(config: ScalafmtReflectConfig): Unit = {
    if (respectVersion) {
      val expected = version.toString
      val obtained = config.version
      if (obtained != expected) {
        throw VersionMismatch(obtained, expected)
      }
    }
  }

  private def moduleInstance(fqn: String): Object = {
    val cls = classLoader.loadClass(fqn)
    val module = cls.getField("MODULE$")
    module.setAccessible(true)
    module.get(null)
  }
}
