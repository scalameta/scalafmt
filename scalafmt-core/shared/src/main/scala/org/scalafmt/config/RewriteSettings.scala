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
    allowInfixPlaceholderArg: Boolean = true,
    neverInfix: Pattern = Pattern.neverInfix
) {
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

  private[config] def forSbt: RewriteSettings =
    neverInfix.forSbt.fold(this)(x => copy(neverInfix = x))
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
