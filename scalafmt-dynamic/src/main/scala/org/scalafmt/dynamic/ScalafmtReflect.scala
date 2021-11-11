package org.scalafmt.dynamic

import com.typesafe.config.ConfigFactory
import java.nio.file.Path
import org.scalafmt.dynamic.exceptions._
import org.scalafmt.dynamic.utils.ReflectUtils._
import scala.util.Failure
import scala.util.Try

case class ScalafmtReflect(
    classLoader: ClassLoader,
    version: ScalafmtVersion
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
      val scalaFmtConfigCls = loadClass("org.scalafmt.config.ScalafmtConfig")
      val configTarget = scalaFmtConfigCls.invokeStatic("intellij")
      Some(new ScalafmtReflectConfig(this)(configTarget))
    } else {
      None
    }
  }

  def parseConfig(path: Path): Try[ScalafmtReflectConfig] =
    parseConfigPost300(path)
      .map { configured =>
        new ScalafmtReflectConfig(this)(configured.invoke("get"))
      }
      .recoverWith { case ReflectionException(e) =>
        Failure(new ScalafmtDynamicError.ConfigParseError(path, e.getMessage))
      }

  private def parseConfigPost300(path: Path): Try[Object] = {
    // scalafmt >= 3.0.0
    Try(scalafmtCls.invokeStatic("parseHoconConfigFile", path.asParam))
      .recoverWith { case _: NoSuchMethodException =>
        parseConfigPre300(path)
      }
  }

  private def parseConfigPre300(path: Path): Try[Object] = {
    val textParam = ConfigFactory.parseFile(path.toFile).root.render().asParam
    // scalafmt >= 1.6.0
    Try(scalafmtCls.invokeStatic("parseHoconConfig", textParam))
      .recoverWith { case _: NoSuchMethodException =>
        parseConfigPre160(textParam)
      }
  }

  private def parseConfigPre160(textParam: (Class[_], String)): Try[Object] = {
    // scalafmt >= v0.7.0-RC1 && scalafmt < 1.6.0
    Try {
      val fromHoconEmptyPath =
        configCls.invokeStatic("fromHoconString$default$2").asParam(optionCls)
      configCls.invokeStatic("fromHoconString", textParam, fromHoconEmptyPath)
    }
  }

  def format(
      code: String,
      config: ScalafmtReflectConfig,
      fileOpt: Option[Path] = None
  ): String = {
    require(this eq config.fmtReflect)

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

  private def moduleInstance(fqn: String): Object = {
    val module = loadClass(fqn).getField("MODULE$")
    module.setAccessible(true)
    module.get(null)
  }
}
