package org.scalafmt.config

import metaconfig.ConfigReader
import metaconfig.Reader
import org.scalafmt.rewrite.{AvoidInfix, Rewrite}

@ConfigReader
case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    redundantBraces: RedundantBracesSettings = RedundantBracesSettings(),
    neverInfix: Pattern = Pattern.neverInfix
) {
  implicit val rewriteReader: Reader[Rewrite] = Rewrite.reader

  implicit val patternReader: Reader[Pattern] = neverInfix.reader

  implicit val curlyReader: Reader[RedundantBracesSettings] =
    redundantBraces.reader
}
