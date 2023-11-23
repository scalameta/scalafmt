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

  def matches(op: String): Boolean =
    includeFilters.forall(_.matches(op)(_.find())) &&
      !excludeFilters.exists(_.matches(op)(_.matches()))
}

object NeverInfixPattern {

  private[config] case class Filter(
      op: Pattern
  ) {
    private def pattern: String = {
      op.pattern()
    }
    def matches(op: String)(
        f: Matcher => Boolean
    ): Boolean =
      f(this.op.matcher(op))
  }

  private object Filter {
    implicit lazy val surface: Surface[Filter] = generic.deriveSurface
    implicit lazy val encoder: ConfEncoder[Filter] =
      ConfEncoder.instance(x => Conf.Str(x.pattern))
    implicit lazy val decoder: ConfDecoderEx[Filter] =
      ConfDecoderEx.fromPartial("String") { case (_, Conf.Str(x)) => parse(x) }

    private def parse(str: String): Configured[Filter] = {
      val op = str
      if (op.isEmpty) Configured.error(s"empty infix op [$str]")
      else {
        Configured.Ok(Filter(op.r.pattern))
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
