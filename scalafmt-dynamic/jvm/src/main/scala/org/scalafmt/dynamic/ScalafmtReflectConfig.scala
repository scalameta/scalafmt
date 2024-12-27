package org.scalafmt.dynamic

import org.scalafmt.dynamic.exceptions.ReflectionException
import org.scalafmt.dynamic.utils.ReflectUtils._

import java.lang.reflect.Constructor
import java.nio.file.Path

import scala.util.Try

//noinspection TypeAnnotation
class ScalafmtReflectConfig private[dynamic] (val fmtReflect: ScalafmtReflect)(
    private[dynamic] val target: Object,
) {
  import ScalafmtReflectConfig._
  import fmtReflect.classLoader
  private val targetCls = target.getClass

  private val dialectCls = classLoader.loadClass("scala.meta.Dialect")
  private val dialectsCls = classLoader.loadClass("scala.meta.dialects.package")

  private val projectField = target.invoke("project")
  private val projectMatcherField = projectField.invoke("matcher")

  private lazy val indentField = Try(
    if (getVersion < ScalafmtVersion(3, 0, 0)) target
      .invoke("continuationIndent")
    else target.invoke("indent"),
  )

  lazy val projectIsGit = Try(projectField.invokeAs[Boolean]("git"))
    .getOrElse(false)

  @inline
  def getVersion = fmtReflect.version

  @inline
  def isIncludedInProject(path: Path): Boolean =
    isIncludedInProject(path.toString)

  def isIncludedInProject(filename: String): Boolean = projectMatcherField
    .invokeAs[Boolean]("matches", filename.asParam)

  private def sbtDialect: Object =
    try dialectsCls.invokeStatic("Sbt")
    catch {
      case ReflectionException(_: NoSuchMethodException) => dialectsCls
          .invokeStatic("Sbt0137")
    }

  lazy val withSbtDialect: Try[ScalafmtReflectConfig] = Try(
    target.invoke("forSbt"),
  ).recover { case ReflectionException(_: NoSuchMethodException) =>
    target.invoke("withDialect", (dialectCls, sbtDialect))
  }.map(withTarget)

  def withoutRewriteRules: ScalafmtReflectConfig =
    if (getVersion < ScalafmtVersion(3, 2, 0)) withoutRewriteRulesPre320
    else withTarget(target.invoke("withoutRewrites"))

  private def withoutRewriteRulesPre320: ScalafmtReflectConfig =
    if (!hasRewriteRulesPre320) this
    else {
      // emulating this.copy(rewrite = RewriteSettings())
      val ctor: Constructor[_] = targetCls.getConstructors()(0)
      val params = ctor.getParameters.map(_.getName)
      val rewriteParamIdx = params.indexOf(rewriteFieldName).ensuring(_ >= 0)
      val publicMethodNames = targetCls.getMethods.view.map(_.getName)
      val publicParams = publicMethodNames.filter(params.contains).toSet
      val fieldValues = params.zipWithIndex.map { case (p, i) =>
        target.invoke(
          if (i == rewriteParamIdx) "apply$default$" + (rewriteParamIdx + 1)
          else if (publicParams.contains(p)) p
          else p + "$access$" + i,
        )
      }
      withTarget(ctor.newInstance(fieldValues: _*).asInstanceOf[Object])
    }

  def hasRewriteRules: Boolean =
    if (getVersion < ScalafmtVersion(3, 2, 0)) hasRewriteRulesPre320
    else target.invokeAs[Boolean]("hasRewrites")

  private def hasRewriteRulesPre320: Boolean = // scalafmt >= v0.4.1
    Try {
      val rules = target.invoke(rewriteFieldName).invoke("rules")
      !rules.invokeAs[Boolean]("isEmpty")
    }.getOrElse(false)

  def tryFormat(code: String, file: Option[Path]): Try[String] = fmtReflect
    .tryFormat(code, this, file)

  def indentMain: Option[Int] =
    if (getVersion < ScalafmtVersion(3, 0, 0)) Some(2)
    else indentField.map(_.invokeAs[Int]("main")).toOption

  def indentCallSite: Option[Int] = indentField.map(_.invokeAs[Int]("callSite"))
    .toOption

  def indentDefnSite: Option[Int] = indentField.map(_.invokeAs[Int]("defnSite"))
    .toOption

  def needGitAutoCRLF: Boolean = getVersion > ScalafmtVersion(3, 8, 2) &&
    target.invokeAs[Boolean]("needGitAutoCRLF")

  // must invoke needGitAutoCRLF first
  def withGitAutoCRLF(value: String): ScalafmtReflectConfig =
    withTarget(target.invoke("withGitAutoCRLF", value.asParam))

  override def equals(obj: Any): Boolean = target.equals(obj)

  override def hashCode(): Int = target.hashCode()

  private def withTarget(obj: Object): ScalafmtReflectConfig =
    new ScalafmtReflectConfig(fmtReflect)(obj)
}

private object ScalafmtReflectConfig {

  val rewriteFieldName = "rewrite"

}
