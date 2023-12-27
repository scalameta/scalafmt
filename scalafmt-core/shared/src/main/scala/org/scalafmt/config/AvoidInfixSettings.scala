package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

import java.util.regex.{Matcher, Pattern}

case class AvoidInfixSettings(
    // partial match
    private[config] val includeFilters: Seq[AvoidInfixSettings.Filter],
    // strict match
    private[config] val excludeFilters: Seq[AvoidInfixSettings.Filter],
    private val excludeScalaTest: Option[Boolean] = None,
    excludePlaceholderArg: Option[Boolean] = None
) {
  // if the user completely redefined (rather than appended), we don't touch
  @inline private def isMainExclude: Boolean =
    excludeStartsWith(AvoidInfixSettings.mainExclude)

  private[config] def forSbtOpt: Option[AvoidInfixSettings] =
    // if the user customized these, we don't touch
    if (isMainExclude) withExtraExclude(AvoidInfixSettings.sbtExclude) else None

  private[config] def forMainOpt: Option[AvoidInfixSettings] =
    if (excludeScalaTest.contains(true)) Some(withTestExclude) else None

  private[config] def forTestOpt: Option[AvoidInfixSettings] =
    if (excludeScalaTest.getOrElse(isMainExclude)) Some(withTestExclude)
    else None

  private def withTestExclude: AvoidInfixSettings =
    getExcludeWithExtra(AvoidInfixSettings.testExclude).fold(
      copy(excludeScalaTest = Some(false))
    )(x => copy(excludeFilters = x, excludeScalaTest = Some(false)))

  @inline
  private def excludeStartsWith(obj: Seq[AvoidInfixSettings.Filter]): Boolean =
    excludeFilters.eq(obj) || excludeFilters.startsWith(obj)

  private def getExcludeWithExtra(
      obj: Seq[AvoidInfixSettings.Filter]
  ): Option[Seq[AvoidInfixSettings.Filter]] =
    if (obj.isEmpty) None
    else {
      val newExclude = (excludeFilters ++ obj).distinct
      val changed = newExclude.lengthCompare(excludeFilters.length) > 0
      if (changed) Some(newExclude) else None
    }

  private[config] def withExtraExclude(
      obj: Seq[AvoidInfixSettings.Filter]
  ): Option[AvoidInfixSettings] =
    getExcludeWithExtra(obj).map(x => copy(excludeFilters = x))

  def matches(lhs: String, op: String): Boolean =
    includeFilters.forall(_.matches(lhs, op)(_.find())) &&
      !excludeFilters.exists(_.matches(lhs, op)(_.matches()))
}

object AvoidInfixSettings {

  private[config] case class Filter(
      lhs: Option[Pattern],
      op: Pattern
  ) {
    private def pattern: String = {
      val opPat = op.pattern()
      lhs.fold(opPat) { lhs => s"${lhs.pattern()}\\.$opPat" }
    }
    def matches(lhs: String, op: String)(
        f: Matcher => Boolean
    ): Boolean =
      f(this.op.matcher(op)) && this.lhs.forall(r => f(r.matcher(lhs)))

    override def equals(obj: Any): Boolean = obj match {
      case x: Filter =>
        x.op.pattern() == op.pattern() && x.lhs.fold(lhs.isEmpty) { y =>
          lhs.exists(_.pattern() == y.pattern())
        }
      case _ => false
    }

    override def hashCode(): Int =
      lhs.fold(0)(_.pattern().##) ^ op.pattern().##
  }

  private[config] object Filter {
    implicit lazy val surface: Surface[Filter] = generic.deriveSurface
    implicit lazy val encoder: ConfEncoder[Filter] =
      ConfEncoder.instance(x => Conf.Str(x.pattern))
    implicit lazy val decoder: ConfDecoderEx[Filter] =
      ConfDecoderEx.fromPartial("String") { case (_, Conf.Str(x)) => parse(x) }

    private def parse(str: String): Configured[Filter] = {
      val idx = str.lastIndexOf("\\.")
      val op = if (idx < 0) str else str.substring(idx + 2)
      if (op.isEmpty) Configured.error(s"empty infix op [$str]")
      else {
        val lhs = if (idx <= 0) None else Some(str.substring(0, idx).r.pattern)
        Configured.Ok(Filter(lhs, op.r.pattern))
      }
    }

    def apply(value: String): Filter = parse(value).get
  }

  implicit lazy val surface: Surface[AvoidInfixSettings] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[AvoidInfixSettings] =
    generic.deriveCodecEx(default).noTypos

  private[config] def mainInclude =
    Seq(
      "[\\w\\d_]+"
    ).map(Filter.apply)

  private[config] val mainExclude =
    Seq(
      "until",
      "to",
      "by",
      "eq",
      "ne"
    ).map(Filter.apply)

  private val testExclude =
    Seq(
      "should.*",
      "contain.*",
      "must.*",
      "in",
      "ignore",
      "be",
      "behavior\\.of",
      "taggedAs",
      "thrownBy",
      "synchronized",
      "have",
      "when",
      "size",
      "only",
      "noneOf",
      "oneElementOf",
      "noElementsOf",
      "atLeastOneElementOf",
      "atMostOneElementOf",
      "allElementsOf",
      "inOrderElementsOf",
      "theSameElementsAs"
    ).map(Filter.apply)

  private[config] val sbtExclude = Seq(
    "cross"
  ).map(Filter.apply)

  private[config] val default = AvoidInfixSettings(
    includeFilters = mainInclude,
    excludeFilters = mainExclude
  )

}
