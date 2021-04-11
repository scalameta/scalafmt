package org.scalafmt.config

import metaconfig._
import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.rewrite.Rewrite

case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    scala3: RewriteScala3Settings = new RewriteScala3Settings,
    redundantBraces: RedundantBracesSettings = RedundantBracesSettings(),
    sortModifiers: SortSettings = SortSettings.default,
    neverInfix: Pattern = Pattern.neverInfix
) {
  private implicit val redundantBracesReader = redundantBraces.reader
  private implicit val patternReader = neverInfix.reader
  private implicit val scala3Decoder = scala3.decoder
  val reader: ConfDecoder[RewriteSettings] = generic.deriveDecoder(this).noTypos
  Rewrite.validateRewrites(rules) match {
    case Nil => // OK
    case errs =>
      throw InvalidScalafmtConfiguration(
        new IllegalArgumentException(
          errs.mkString("\n")
        )
      )
  }

  def rulesChanged(v2: RewriteSettings): Boolean = {
    this != v2.copy(scala3 = scala3) // ignore scala3 changes
  }

}

object RewriteSettings {
  implicit lazy val surface: generic.Surface[RewriteSettings] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[RewriteSettings] =
    generic.deriveEncoder

}
