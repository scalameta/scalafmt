package org.scalafmt.config

import metaconfig._
import org.scalafmt.rewrite._

case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    scala3: RewriteScala3Settings = RewriteScala3Settings.default,
    insertBraces: RewriteSettings.InsertBraces = RewriteSettings.InsertBraces(),
    redundantBraces: RedundantBracesSettings = RedundantBracesSettings.default,
    redundantParens: RedundantParensSettings = RedundantParensSettings.default,
    sortModifiers: SortSettings = SortSettings.default,
    imports: Imports.Settings = Imports.Settings(),
    preferCurlyFors: PreferCurlyFors.Settings = PreferCurlyFors.Settings(),
    trailingCommas: TrailingCommas = TrailingCommas(),
    @annotation.DeprecatedName(
      "allowInfixPlaceholderArg",
      "Use `avoidInfix.excludePlaceholderArg` instead",
      "3.8.0"
    )
    private val allowInfixPlaceholderArg: Boolean = true,
    @annotation.ExtraName("neverInfix")
    avoidInfix: AvoidInfixSettings = AvoidInfixSettings.default
) {
  def isAllowInfixPlaceholderArg: Boolean =
    avoidInfix.excludePlaceholderArg.getOrElse(allowInfixPlaceholderArg)

  def withoutRewrites: RewriteSettings =
    copy(rules = Nil, trailingCommas = trailingCommas.withoutRewrites)

  def rewriteFactoryRules: Seq[RewriteFactory] =
    rules.collect { case x: RewriteFactory => x }

  def rulesChanged(v2: RewriteSettings): Option[Seq[String]] = {
    val v1rules = rewriteFactoryRules.toSet
    val v2rules = v2.rewriteFactoryRules.toSet
    val intersection = v1rules & v2rules
    val missing = (v1rules | v2rules).diff(intersection).toSeq
    val changed = intersection.toSeq.filter(_.hasChanged(this, v2))
    Some((missing ++ changed).map(_.getClass.getSimpleName).sorted)
      .filter(_.nonEmpty)
  }

  private[config] def forSbtOpt: Option[RewriteSettings] =
    avoidInfix.forSbtOpt.map(x => copy(avoidInfix = x))

  private[config] def forMainOpt: Option[RewriteSettings] =
    avoidInfix.forMainOpt.map(x => copy(avoidInfix = x))

  private[config] def forTestOpt: Option[RewriteSettings] =
    avoidInfix.forTestOpt.map(x => copy(avoidInfix = x))
}

object RewriteSettings {

  val default = RewriteSettings()

  implicit lazy val surface: generic.Surface[RewriteSettings] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[RewriteSettings] =
    generic.deriveEncoder

  implicit lazy val decoder: ConfDecoderEx[RewriteSettings] =
    generic.deriveDecoderEx(default).noTypos.flatMap {
      Imports.validateImports
    }

  case class InsertBraces(
      minLines: Int = 0,
      allBlocks: Boolean = false
  )

  private[RewriteSettings] object InsertBraces {
    implicit val surface: generic.Surface[InsertBraces] = generic.deriveSurface
    implicit val codec: ConfCodecEx[InsertBraces] =
      generic.deriveCodecEx(new InsertBraces)
  }

}
