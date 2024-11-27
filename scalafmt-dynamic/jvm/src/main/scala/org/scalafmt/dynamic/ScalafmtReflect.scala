package org.scalafmt.dynamic

import org.scalafmt.dynamic.exceptions._
import org.scalafmt.dynamic.utils.ReflectUtils._

import java.io.Closeable
import java.nio.file.Path

import scala.util.Failure
import scala.util.Try

import com.typesafe.config.ConfigFactory

case class ScalafmtReflect(classLoader: ClassLoader, version: ScalafmtVersion)
    extends Closeable {
  import classLoader.loadClass

  // FIXME: the class does not exist for version old versions, e.g. v0.2.8
  private val formattedCls = loadClass("org.scalafmt.Formatted")
  private val scalaSetCls = loadClass("scala.collection.immutable.Set")
  private val optionCls = loadClass("scala.Option")
  private lazy val configCls = loadClass("org.scalafmt.config.Config")
  private val scalafmtCls = loadClass("org.scalafmt.Scalafmt")

  private val parseExceptionCls = loadClass("scala.meta.parsers.ParseException")
  private val tokenizeExceptionCls =
    loadClass("scala.meta.tokenizers.TokenizeException")

  private val defaultScalaFmtConfig = scalafmtCls
    .invokeStatic("format$default$2")
  private val emptyRange = scalafmtCls.invokeStatic("format$default$3")

  private val formattedGet = formattedCls.getMethod("get")
  private val formatMethod = scalafmtCls.getMethod(
    "format",
    classOf[String],
    defaultScalaFmtConfig.getClass,
    scalaSetCls,
  )
  private val formatMethodWithFilename = Try(scalafmtCls.getMethod(
    "format",
    classOf[String],
    defaultScalaFmtConfig.getClass,
    scalaSetCls,
    classOf[String],
  )).toOption

  lazy val intellijScalaFmtConfig: Option[ScalafmtReflectConfig] =
    if (version < ScalafmtVersion(1, 5, 1)) None
    else {
      val scalaFmtConfigCls = loadClass("org.scalafmt.config.ScalafmtConfig")
      val configTarget = scalaFmtConfigCls.invokeStatic("intellij")
      Some(new ScalafmtReflectConfig(this)(configTarget))
    }

  private def parseConfigWith(
      f: => Try[Object],
      path: Path = null,
  ): Try[ScalafmtReflectConfig] = {
    import ScalafmtDynamicError.ConfigParseError
    @inline
    def fail(e: Throwable) =
      Failure(new ConfigParseError(path, e.getMessage, e.getCause))
    f.map(configured => new ScalafmtReflectConfig(this)(configured.invoke("get")))
      .recoverWith {
        case e: ReflectiveOperationException =>
          fail(Option(e.getCause).getOrElse(e))
        case e => fail(e)
      }
  }

  def parseConfig(path: Path): Try[ScalafmtReflectConfig] =
    parseConfigWith(parseConfigPost300(path), path)

  def parseConfigFromString(
      path: Path,
      text: String,
  ): Try[ScalafmtReflectConfig] = parseConfigWith(parseConfigPre300(text), path)

  def parseConfigFromString(text: String): Try[ScalafmtReflectConfig] =
    parseConfigWith(parseConfigPre300(text))

  private def parseConfigPost300(path: Path): Try[Object] =
    if (version < ScalafmtVersion(3, 0, 0, 7)) parseConfigPre300(path)
    else Try(scalafmtCls.invokeStatic("parseHoconConfigFile", path.asParam))

  private def parseConfigPre300(path: Path): Try[Object] =
    parseConfigPre300(ConfigFactory.parseFile(path.toFile).root.render())

  private def parseConfigPre300(text: String): Try[Object] = {
    val textParam = text.asParam
    if (version < ScalafmtVersion(1, 6, 0, 1)) parseConfigPre160(textParam)
    else Try(scalafmtCls.invokeStatic("parseHoconConfig", textParam))
  }

  private def parseConfigPre160(textParam: (Class[_], Object)): Try[Object] =
    // scalafmt >= v0.7.0-RC1 && scalafmt < 1.6.0
    Try {
      val fromHoconEmptyPath = configCls
        .invokeStatic("fromHoconString$default$2").asParam(optionCls)
      configCls.invokeStatic("fromHoconString", textParam, fromHoconEmptyPath)
    }

  def tryFormat(
      code: String,
      config: ScalafmtReflectConfig,
      fileOpt: Option[Path] = None,
  ): Try[String] = {
    require(this eq config.fmtReflect)

    Try {
      val formatted = (formatMethodWithFilename, fileOpt) match {
        case (Some(method), Some(file)) =>
          val filename = file.toString
          method.invoke(null, code, config.target, emptyRange, filename)
        case _ => formatMethod.invoke(null, code, config.target, emptyRange)
      }
      if (version < ScalafmtVersion(2, 3, 0))
        moduleInstance("scala.meta.internal.tokenizers.PlatformTokenizerCache$")
          .invoke("megaCache").invoke("clear")
      formattedGet.invoke(formatted).asInstanceOf[String]
    }.recoverWith {
      case ReflectionException(e)
          if tokenizeExceptionCls.isInstance(e) ||
            parseExceptionCls.isInstance(e) =>
        val pos = e.invoke("pos")
        val range = positionRange(pos)
        val shortMessage = e.invokeAs[String]("shortMessage")
        val exception = PositionExceptionImpl(
          fileOpt.orNull,
          code,
          shortMessage,
          e.getMessage,
          range,
          e,
        )
        Failure(exception)
    }
  }

  private def positionRange(pos: Object): RangePosition =
    try RangePosition(
        pos.invokeAs[Int]("start"),
        pos.invokeAs[Int]("startLine"),
        pos.invokeAs[Int]("startColumn"),
        pos.invokeAs[Int]("end"),
        pos.invokeAs[Int]("endLine"),
        pos.invokeAs[Int]("endColumn"),
      )
    catch {
      case _: ReflectiveOperationException | _: ClassCastException =>
        val start = pos.invoke("start")
        val end = pos.invoke("end")
        RangePosition(
          start.invokeAs[Int]("offset"),
          start.invokeAs[Int]("line"),
          start.invokeAs[Int]("column"),
          end.invokeAs[Int]("offset"),
          end.invokeAs[Int]("line"),
          end.invokeAs[Int]("column"),
        )
    }

  private def moduleInstance(fqn: String): Object = {
    val module = loadClass(fqn).getField("MODULE$")
    module.setAccessible(true)
    module.get(null)
  }

  override def close(): Unit = classLoader match {
    case x: Closeable => x.close()
    case _ =>
  }
}
