package org.scalafmt.config

import metaconfig._
import metaconfig.generic.Surface

import java.util.regex.{Matcher, Pattern}

case class NeverInfixPattern(
    private val includeFilters: Seq[NeverInfixPattern.Filter], // partial match
    private val excludeFilters: Seq[NeverInfixPattern.Filter] // strict match
) {
  private[config] def forSbt: Option[NeverInfixPattern] =
    // if the user customized these, we don't touch
    if (excludeFilters ne NeverInfixPattern.default.excludeFilters) None
    else Some(copy(excludeFilters = NeverInfixPattern.sbtExclude))

  def matches(lhs: String, op: String): Boolean =
    includeFilters.forall(_.matches(lhs, op)(_.find())) &&
      !excludeFilters.exists(_.matches(lhs, op)(_.matches()))
}

object NeverInfixPattern {

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
  }

  private object Filter {
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

  implicit lazy val surface: Surface[NeverInfixPattern] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[NeverInfixPattern] =
    generic.deriveCodecEx(default).noTypos
  val default = NeverInfixPattern(
    Seq("[\\w\\d_]+").map(Filter.apply),
    Seq(
      "until",
      "to",
      "by",
      "eq",
      "ne",
      "should.*",
      "contain.*",
      "must.*",
      "in",
      "ignore",
      "be",
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
  )

  private val sbtExclude = Seq(
    "cross"
  ).map(Filter.apply) ++ default.excludeFilters
}
