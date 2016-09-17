package org.scalafmt.config

import metaconfig.ConfigReader
import metaconfig.Reader
import org.scalafmt.rewrite.Rewrite

@ConfigReader
case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    redundantBraces: RedundantBracesSettings = RedundantBracesSettings()
) {
  implicit val rewriteReader: Reader[Rewrite] = Rewrite.reader

  implicit val curlyReader: Reader[RedundantBracesSettings] =
    redundantBraces.reader
}
