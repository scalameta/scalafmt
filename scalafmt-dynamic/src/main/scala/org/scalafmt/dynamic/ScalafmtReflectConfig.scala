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
  import ScalafmtReflectConfig._
  import fmtReflect.classLoader
  private val targetCls = target.getClass

  private val dialectCls = classLoader.loadClass("scala.meta.Dialect")
  private val dialectsCls = classLoader.loadClass("scala.meta.dialects.package")

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
    if (getVersion < ScalafmtVersion(3, 2, 0)) withoutRewriteRulesPre320
    else new ScalafmtReflectConfig(fmtReflect)(target.invoke("withoutRewrites"))
  }

  private def withoutRewriteRulesPre320: ScalafmtReflectConfig =
    if (!hasRewriteRulesPre320) this
    else {
      // emulating this.copy(rewrite = RewriteSettings())
      val constructor: Constructor[_] = targetCls.getConstructors()(0)
      val constructorParams = constructor.getParameters.map(_.getName)
      val rewriteParamIdx =
        constructorParams.indexOf(rewriteFieldName).ensuring(_ >= 0)
      val emptyRewrites =
        target.invoke("apply$default$" + (rewriteParamIdx + 1))
      val fieldsValues = constructorParams.map(param => target.invoke(param))
      fieldsValues(rewriteParamIdx) = emptyRewrites
      new ScalafmtReflectConfig(fmtReflect)(
        constructor.newInstance(fieldsValues: _*).asInstanceOf[Object]
      )
    }

  def hasRewriteRules: Boolean = {
    if (getVersion < ScalafmtVersion(3, 2, 0)) hasRewriteRulesPre320
    else target.invokeAs[Boolean]("hasRewrites")
  }

  private def hasRewriteRulesPre320: Boolean = // scalafmt >= v0.4.1
    Try {
      val rules = target.invoke(rewriteFieldName).invoke("rules")
      !rules.invokeAs[Boolean]("isEmpty")
    }.getOrElse(false)

  def format(code: String, file: Option[Path]): String =
    fmtReflect.format(code, this, file)

  override def equals(obj: Any): Boolean = target.equals(obj)

  override def hashCode(): Int = target.hashCode()
}

private object ScalafmtReflectConfig {

  val rewriteFieldName = "rewrite"

}
