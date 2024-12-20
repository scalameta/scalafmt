package org.scalafmt.config

import org.scalafmt.rewrite._

import metaconfig._

@annotation.SectionRename("neverInfix", "avoidInfix") // renamed in v3.8.0
@annotation.SectionRename(
  "allowInfixPlaceholderArg",
  "avoidInfix.excludePlaceholderArg",
) // deprecated since v3.8.0
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
    avoidInfix: AvoidInfixSettings = AvoidInfixSettings.default,
) {
  def withoutRewrites: RewriteSettings =
    copy(rules = Nil, trailingCommas = trailingCommas.withoutRewrites)

  def rewriteFactoryRules: Seq[RewriteFactory] = rules
    .collect { case x: RewriteFactory => x }

  def rulesChanged(v2: RewriteSettings): Option[Seq[String]] = {
    val v1rules = rewriteFactoryRules.toSet
    val v2rules = v2.rewriteFactoryRules.toSet
    val intersection = v1rules & v2rules
    val missing = (v1rules | v2rules).diff(intersection).toSeq
    val changed = intersection.toSeq.filter(_.hasChanged(this, v2))
    Some((missing ++ changed).map(_.getClass.getSimpleName).sorted)
      .filter(_.nonEmpty)
  }

  private[config] def forSbtOpt: Option[RewriteSettings] = avoidInfix.forSbtOpt
    .map(x => copy(avoidInfix = x))

  private[config] def forMainOpt: Option[RewriteSettings] = avoidInfix
    .forMainOpt.map(x => copy(avoidInfix = x))

  private[config] def forTestOpt: Option[RewriteSettings] = avoidInfix
    .forTestOpt.map(x => copy(avoidInfix = x))

  def bracesToParensForOneLineApply = redundantBraces.parensForOneLineApply &&
    rules.contains(RedundantBraces)

}

object RewriteSettings {

  val default = RewriteSettings()

  implicit lazy val surface: generic.Surface[RewriteSettings] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[RewriteSettings] =
    generic.deriveEncoder

  implicit lazy val decoder: ConfDecoderEx[RewriteSettings] = generic
    .deriveDecoderEx(default).noTypos.flatMap(Imports.validateImports)
    .detectSectionRenames

  case class InsertBraces(minLines: Int = 0, allBlocks: Boolean = false)

  private[RewriteSettings] object InsertBraces {
    implicit val surface: generic.Surface[InsertBraces] = generic.deriveSurface
    implicit val codec: ConfCodecEx[InsertBraces] = generic
      .deriveCodecEx(new InsertBraces)
  }

}
