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

  private implicit class ImplicitStyle(private val style: ScalafmtConfig)
      extends AnyVal {

    def withSource(source: Newlines.SourceHints): ScalafmtConfig = style
      .copy(newlines = style.newlines.copy(source = source))

    def withRewrites(): ScalafmtConfig = style.copy(rewrite =
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

    def withOverflow(types: Newlines.AvoidForSimpleOverflow*): ScalafmtConfig =
      style.copy(newlines = style.newlines.copy(avoidForSimpleOverflow = types))

  }

  private val baseKeepStyle = baseClassicStyle.withSource(Newlines.keep)
  private val baseFoldStyle = baseClassicStyle.withSource(Newlines.fold)

  val styles: Map[String, ScalafmtConfig] = SortedMap(
    "classic" -> baseClassicStyle,
    "classicWithRewrites" -> baseClassicStyle.withRewrites(),
    "classicWithAlign" -> baseClassicStyle.withAlign(Align.most),
    "keep" -> baseKeepStyle,
    "keepWithRewrites" -> baseKeepStyle.withRewrites(),
    "keepWithScalaJS" -> baseKeepStyle.forScalaJs,
    "fold" -> baseFoldStyle,
    "foldWithRewritesAndOverflow" -> baseFoldStyle.withRewrites()
      .withOverflow(Newlines.AvoidForSimpleOverflow.all: _*),
    "unfold" -> baseClassicStyle.withSource(Newlines.unfold),
  )

}
