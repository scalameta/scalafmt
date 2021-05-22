package org.scalafmt.config

import metaconfig._
import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.rewrite.Rewrite
import org.scalafmt.rewrite.RewriteFactory

case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    scala3: RewriteScala3Settings = new RewriteScala3Settings,
    redundantBraces: RedundantBracesSettings = RedundantBracesSettings(),
    sortModifiers: SortSettings = SortSettings.default,
    neverInfix: Pattern = Pattern.neverInfix
) {
  Rewrite.validateRewrites(rules) match {
    case Nil => // OK
    case errs =>
      throw InvalidScalafmtConfiguration(
        new IllegalArgumentException(
          errs.mkString("\n")
        )
      )
  }

  def rewriteFactoryRules: Seq[RewriteFactory] =
    rules.collect { case x: RewriteFactory => x }

  def rulesChanged(v2: RewriteSettings): Option[Seq[String]] = {
    val v1rules = rewriteFactoryRules.toSet
    val v2rules = v2.rewriteFactoryRules.toSet
    val intersection = v1rules & v2rules
    val missing = (v1rules | v2rules).diff(intersection).toSeq
    val changed = intersection.toSeq.filter(_.hasChanged(this, v2))
    Some((missing ++ changed).map(_.toString).sorted).filter(_.nonEmpty)
  }

}

object RewriteSettings {
  implicit lazy val surface: generic.Surface[RewriteSettings] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[RewriteSettings] =
    generic.deriveCodecEx(RewriteSettings()).noTypos

}
