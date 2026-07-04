package org.scalafmt.benchmarks

import org.scalafmt.config._

/** Config combinations used by the benchmarks, mirroring the community-test
  * `TestStyles` source variants. `TestStyles` itself is `private[community]`
  * and test-scoped, so it can't be reused here; this replicates the parts
  * relevant to benchmarking (the four `newlines.source` modes, where `classic`
  * is the unspecified/default source), on a base with the same stability tweaks
  * (`escapeInPathologicalCases` off, no `maxStateVisits` cap, `docstrings`
  * kept).
  */
object BenchStyles {

  private val base = {
    val d = ScalafmtConfig.default
    d.copy(
      docstrings = d.docstrings.copy(wrap = Docstrings.Wrap.keep),
      newlines = d.newlines.copy(infix =
        d.newlines.infix.copy(termSite =
          Newlines.Infix.Site.default.copy(maxCountPerFileForKeep = Some(300)),
        ),
      ),
      runner = d.runner.copy(
        maxStateVisits = None,
        optimizer = d.runner.optimizer.copy(escapeInPathologicalCases = false),
      ),
    )
  }

  private def withSource(s: Newlines.SourceHints): ScalafmtConfig = base
    .copy(newlines = base.newlines.copy(source = s))

  /** `classic` == unspecified/default `newlines.source`. */
  val byName: Map[String, ScalafmtConfig] = Map(
    "classic" -> base,
    "keep" -> withSource(Newlines.keep),
    "fold" -> withSource(Newlines.fold),
    "unfold" -> withSource(Newlines.unfold),
  )

}
