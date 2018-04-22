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
