package org.scalafmt.dynamic

import java.lang.reflect.Constructor
import java.nio.file.Path

import org.scalafmt.dynamic.exceptions.ReflectionException
import org.scalafmt.dynamic.utils.ReflectUtils._

import scala.util.Try

//noinspection TypeAnnotation
class ScalafmtReflectConfig private[dynamic] (val fmtReflect: ScalafmtReflect)(
    private[dynamic] val target: Object
) {
  import fmtReflect.classLoader
  private val targetCls = target.getClass
  private val constructor: Constructor[_] = targetCls.getConstructors()(0)
  private val constructorParams = constructor.getParameters.map(_.getName)
  private val rewriteParamIdx =
    constructorParams.indexOf("rewrite").ensuring(_ >= 0)
  private val emptyRewrites =
    target.invoke("apply$default$" + (rewriteParamIdx + 1))

  private val dialectCls = classLoader.loadClass("scala.meta.Dialect")
  private val dialectsCls = classLoader.loadClass("scala.meta.dialects.package")

  private val rewriteRulesMethod = Try(targetCls.getMethod("rewrite")).toOption

  @inline def getVersion = fmtReflect.version

  def isIncludedInProject(filename: String): Boolean = {
    val matcher = target.invoke("project").invoke("matcher")
    matcher.invokeAs[java.lang.Boolean]("matches", filename.asParam)
  }

  private def sbtDialect: Object = {
    try dialectsCls.invokeStatic("Sbt")
    catch {
      case ReflectionException(_: NoSuchMethodException) =>
        dialectsCls.invokeStatic("Sbt0137")
    }
  }

  lazy val withSbtDialect: ScalafmtReflectConfig =
    new ScalafmtReflectConfig(fmtReflect)(
      try target.invoke("forSbt")
      catch {
        case ReflectionException(_: NoSuchMethodException) =>
          target.invoke("withDialect", (dialectCls, sbtDialect))
      }
    )

  def withoutRewriteRules: ScalafmtReflectConfig = {
    if (hasRewriteRules) {
      // emulating this.copy(rewrite = RewriteSettings())
      val fieldsValues = constructorParams.map(param => target.invoke(param))
      fieldsValues(rewriteParamIdx) = emptyRewrites
      new ScalafmtReflectConfig(fmtReflect)(
        constructor.newInstance(fieldsValues: _*).asInstanceOf[Object]
      )
    } else {
      this
    }
  }

  def hasRewriteRules: Boolean = {
    rewriteRulesMethod match {
      case Some(method) =>
        // scalafmt >= v0.4.1
        val rewriteSettings = method.invoke(target)
        !rewriteSettings.invoke("rules").invokeAs[Boolean]("isEmpty")
      case None =>
        false
    }
  }

  def format(code: String, file: Option[Path]): String =
    fmtReflect.format(code, this, file)

  override def equals(obj: Any): Boolean = target.equals(obj)

  override def hashCode(): Int = target.hashCode()
}
