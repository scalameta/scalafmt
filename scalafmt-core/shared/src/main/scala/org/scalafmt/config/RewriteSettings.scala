package org.scalafmt.config

import metaconfig._
import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.rewrite.{AvoidInfix, Rewrite}

@DeriveConfDecoder
case class RewriteSettings(
    rules: Seq[Rewrite] = Nil,
    @Recurse redundantBraces: RedundantBracesSettings =
      RedundantBracesSettings(),
    @Recurse neverInfix: Pattern = Pattern.neverInfix,
    importGroups: List[String] = Nil
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

}
