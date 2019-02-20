package org.scalafmt.dynamic

import java.lang.reflect.Constructor

import org.scalafmt.dynamic.exceptions.ReflectionException
import org.scalafmt.dynamic.utils.ReflectUtils._

import scala.util.Try

//noinspection TypeAnnotation
class ScalafmtReflectConfig private[dynamic] (
    val fmtReflect: ScalafmtReflect,
    private[dynamic] val target: Object,
    private val classLoader: ClassLoader
) {
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

  private val continuationIndentMethod =
    Try(targetCls.getMethod("continuationIndent")).toOption
  private val continuationIndentCallSiteMethod =
    Try(targetCls.getMethod("continuationIndentCallSite")).toOption
  private val continuationIndentDefnSiteMethod =
    Try(targetCls.getMethod("continuationIndentDefnSite")).toOption
  private val DefaultIndentCallSite = 2
  private val DefaultIndentDefnSite = 4

  val version: String = {
    target.invokeAs[String]("version").trim
  }

  def isIncludedInProject(filename: String): Boolean = {
    val matcher = target.invoke("project").invoke("matcher")
    matcher.invokeAs[java.lang.Boolean]("matches", filename.asParam)
  }

  private val sbtDialect: Object = {
    try dialectsCls.invokeStatic("Sbt")
    catch {
      case ReflectionException(_: NoSuchMethodException) =>
        dialectsCls.invokeStatic("Sbt0137")
    }
  }

  def withSbtDialect: ScalafmtReflectConfig = {
    // TODO: maybe hold loaded classes in some helper class not to reload them each time during copy?
    val newTarget = target.invoke("withDialect", (dialectCls, sbtDialect))
    new ScalafmtReflectConfig(fmtReflect, newTarget, classLoader)
  }

  def withoutRewriteRules: ScalafmtReflectConfig = {
    if (hasRewriteRules) {
      // emulating this.copy(rewrite = RewriteSettings())
      val fieldsValues = constructorParams.map(param => target.invoke(param))
      fieldsValues(rewriteParamIdx) = emptyRewrites
      val targetNew =
        constructor.newInstance(fieldsValues: _*).asInstanceOf[Object]
      new ScalafmtReflectConfig(fmtReflect, targetNew, classLoader)
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

  val continuationIndentCallSite: Int = {
    continuationIndentMethod match {
      case Some(method) => // scalafmt >= v0.4
        val indentsObj = method.invoke(target)
        indentsObj.invokeAs[Int]("callSite")
      case None =>
        continuationIndentCallSiteMethod match {
          case Some(method) => // scalafmt >= v0.2.0
            method.invoke(target).asInstanceOf[Int]
          case None =>
            DefaultIndentCallSite
        }
    }
  }

  val continuationIndentDefnSite: Int = {
    continuationIndentMethod match {
      case Some(method) =>
        val indentsObj = method.invoke(target)
        indentsObj.invokeAs[Int]("defnSite")
      case None =>
        continuationIndentDefnSiteMethod match {
          case Some(method) =>
            method.invoke(target).asInstanceOf[Int]
          case None =>
            DefaultIndentDefnSite
        }
    }
  }

  override def equals(obj: Any): Boolean = target.equals(obj)

  override def hashCode(): Int = target.hashCode()
}
