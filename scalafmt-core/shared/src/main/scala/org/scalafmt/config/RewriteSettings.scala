package org.scalafmt.config

import metaconfig._
import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.rewrite.{AvoidInfix, Rewrite}

case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    redundantBraces: RedundantBracesSettings = RedundantBracesSettings(),
    sortModifiers: SortSettings = SortSettings.default,
    neverInfix: Pattern = Pattern.neverInfix
) {
  private implicit val sortModifiersReader = sortModifiers.reader
  private implicit val redundantBracesReader = redundantBraces.reader
  private implicit val patternReader = neverInfix.reader
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

}

object RewriteSettings {
  implicit val surface: generic.Surface[RewriteSettings] =
    generic.deriveSurface
}
