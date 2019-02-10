package org.scalafmt.dynamic

import org.scalafmt.dynamic.ReflectUtils._

class ScalafmtConfigReflect private[dynamic] (
    private[dynamic] val target: Object,
    private[dynamic] val classLoader: ClassLoader
) {
  private val dialectCls = classLoader.loadClass("scala.meta.Dialect")

  def version: String = {
    target.invokeAs[String]("version")
  }

  def isIncludedInProject(filename: String): Boolean = {
    val matcher = target.invoke("project").invoke("matcher")
    matcher.invokeAs[java.lang.Boolean]("matches", filename.asParam)
  }

  def withDialect(sbtDialect: Object): ScalafmtConfigReflect = {
    val newTarget = target.invoke("withDialect", (dialectCls, sbtDialect))
    new ScalafmtConfigReflect(newTarget, classLoader)
  }

}
