package org.scalafmt.community.common

import org.scalafmt.config._
import org.scalafmt.rewrite._

import scala.collection.immutable.SortedMap

object TestStyles {

  private val baseClassicStyle = {
    val base = ScalafmtConfig.default
    val isWin = System.lineSeparator() == "\r\n"
    base.copy(
      docstrings = base.docstrings.copy(wrap = Docstrings.Wrap.keep),
      project = base.project
        .copy(git = true, layout = Some(ProjectFiles.Layout.StandardConvention)),
      lineEndings = Some(if (isWin) LineEndings.windows else LineEndings.unix),
      runner = base.runner.copy(
        maxStateVisits = None,
        optimizer = base.runner.optimizer.copy(escapeInPathologicalCases = false),
      ),
    )
  }

  private def withSource(source: Newlines.SourceHints) = baseClassicStyle
    .copy(newlines = baseClassicStyle.newlines.copy(source = source))

  private def withRewrites(style: ScalafmtConfig) = style.copy(rewrite =
    style.rewrite.copy(
      rules = Seq(RedundantParens, RedundantBraces, SortModifiers, AvoidInfix),
      scala3 = style.rewrite.scala3.copy(
        convertToNewSyntax = true,
        removeOptionalBraces = RewriteScala3Settings.RemoveOptionalBraces.yes,
        insertEndMarkerMinLines = 5,
      ),
      redundantBraces = RedundantBracesSettings.all,
      redundantParens = RedundantParensSettings.all,
    ),
  )

  private val baseKeepStyle = withSource(Newlines.keep)

  val styles = SortedMap(
    "classic" -> baseClassicStyle,
    "classicWithRewrites" -> withRewrites(baseClassicStyle),
    "classicWithAlign" -> baseClassicStyle.withAlign(Align.most),
    "keep" -> baseKeepStyle,
    "keepWithRewrites" -> withRewrites(baseKeepStyle),
    "keepWithAlign" -> baseKeepStyle.withAlign(Align.most),
    "keepWithScalaJS" -> baseKeepStyle.forScalaJs,
    "fold" -> withSource(Newlines.fold),
    "unfold" -> withSource(Newlines.unfold),
  )

}
