package org.scalafmt.config

import metaconfig._
import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.rewrite.{AvoidInfix, Rewrite}

@DeriveConfDecoder
case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    redundantBraces: RedundantBracesSettings = RedundantBracesSettings(),
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
  implicit val rewriteReader: ConfDecoder[Rewrite] = Rewrite.reader

  implicit val patternReader: ConfDecoder[Pattern] = neverInfix.reader

  implicit val curlyReader: ConfDecoder[RedundantBracesSettings] =
    redundantBraces.reader
}
