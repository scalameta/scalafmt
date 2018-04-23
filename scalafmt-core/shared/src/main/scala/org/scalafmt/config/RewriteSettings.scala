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
    @Recurse sortModifiers: SortSettings = SortSettings.default
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

  if (sortModifiers.order.distinct.length != 8)
    throw InvalidScalafmtConfiguration(
      new IllegalArgumentException(
        "'sortModifiers.order', if specified, it has to contain all of the following values in the order you wish them sorted:" +
          """["private", "protected" , "abstract", "final", "sealed", "implicit", "override", "lazy"]"""
      )
    )

}
