package org.scalafmt.community.common

import org.scalafmt.config.RedundantBracesSettings._
import org.scalafmt.config._
import org.scalafmt.rewrite._

import scala.collection.immutable.SortedMap

private[community] object TestStyles {

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

    def withRewrites(
        oneStatApply: OneStatApply = OneStatApply.default,
    ): ScalafmtConfig = style.copy(rewrite =
      style.rewrite.copy(
        rules = Seq(RedundantParens, RedundantBraces, SortModifiers, AvoidInfix),
        scala3 = style.rewrite.scala3.copy(
          convertToNewSyntax = true,
          removeOptionalBraces = RewriteScala3Settings.RemoveOptionalBraces.yes,
          insertEndMarkerMinLines = 5,
        ),
        redundantBraces = RedundantBracesSettings.all
          .copy(maxBreaks = Int.MaxValue, oneStatApply = oneStatApply),
        redundantParens = RedundantParensSettings.all,
      ),
    )

    def withOverflow(types: Newlines.AvoidForSimpleOverflow*): ScalafmtConfig =
      style.copy(newlines = style.newlines.copy(avoidForSimpleOverflow = types))

  }

  private val baseKeepStyle = baseClassicStyle.withSource(Newlines.keep)
  private val baseFoldStyle = baseClassicStyle.withSource(Newlines.fold)
  private val baseUnfoldStyle = baseClassicStyle.withSource(Newlines.unfold)

  val classic = baseClassicStyle
  val classicWithRewrites = baseClassicStyle.withRewrites()
  val classicWithAlign = baseClassicStyle.withAlign(Align.most)
  val keep = baseKeepStyle
  val keepWithRewrites = baseKeepStyle.withRewrites()
  val keepWithAlign = baseKeepStyle.withAlign(Align.most)
  val keepWithScalaJS = baseKeepStyle.forScalaJs
  val fold = baseFoldStyle
  val foldWithRewritesAndOverflow = baseFoldStyle.withRewrites(oneStatApply =
    OneStatApply(parensMaxSpan = 200, bracesMinSpan = 200),
  ).withOverflow(Newlines.AvoidForSimpleOverflow.all.map(_.value): _*)
  val unfold = baseUnfoldStyle
  val unfoldWithRewritesAndOverflow = baseUnfoldStyle
    .withRewrites(oneStatApply =
      OneStatApply(parensMaxSpan = 100, bracesMinSpan = 100),
    ).withOverflow(Newlines.AvoidForSimpleOverflow.all.map(_.value): _*)

  val stylesWithLabels = Seq[sourcecode.Text[ScalafmtConfig]](
    classic,
    classicWithRewrites,
    classicWithAlign,
    keep,
    keepWithRewrites,
    keepWithAlign,
    keepWithScalaJS,
    fold,
    foldWithRewritesAndOverflow,
    unfold,
    unfoldWithRewritesAndOverflow,
  )
  val styles: Map[String, ScalafmtConfig] =
    SortedMap(stylesWithLabels.map(x => x.source -> x.value): _*)

}
