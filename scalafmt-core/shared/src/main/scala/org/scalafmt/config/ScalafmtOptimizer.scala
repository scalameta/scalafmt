package org.scalafmt.config

import ScalafmtOptimizer._
import metaconfig._

/** Configuration for scalafmt optimizations.
  *
  * @param dequeueOnNewStatements
  *   Clear the search queue on new statements.
  * @param escapeInPathologicalCases
  *   Use heuristics to escape when the search state grows out of bounds. An
  *   optimization that trades off optimal formatting output in order to
  *   complete in a reasonable time. Used as a last resort.
  * @param maxVisitsPerToken
  *   Visit the same formatToken at most [[maxVisitsPerToken]] times.
  * @param maxDepth
  *   Maximum depth of recursion.
  * @param acceptOptimalAtHints
  *   Whether to listen to optimalAt fields in Splits.
  * @param disableOptimizationsInsideSensitiveAreas
  *   Do not optimize inside certain areas such as term apply.
  * @param pruneSlowStates
  *   Eliminate solutions that move slower than other solutions.
  *   - If a solution reaches a point X first and other solution that reaches
  *     the same point later, the second solution is ignored unless it can be
  *     verified not to be worse (see
  *     [[org.scalafmt.internal.State.possiblyBetter]]).
  *   - Note. This affects the output positively because it breaks a tie between
  *     two equally expensive solutions by eliminating the slower one.
  *   - Example: solution 1 is preferred even though both solutions cost the
  *     same:
  *     {{{
  *       // solution 1
  *       a + b +
  *       c + d
  *       // solution 2
  *       a +
  *       b + c + d
  *     }}}
  * @param recurseOnBlocks
  *   Recursively format { ... } blocks inside no optimization zones. By
  *   starting a new search queue, we can perform aggressive optimizations
  *   inside optimizations zones.
  */
@annotation.SectionRename("forceConfigStyleMinSpan", "callSite.minSpan") // v3.8.2
@annotation.SectionRename("forceConfigStyleMinArgCount", "callSite.minCount") // v3.8.2
@annotation.SectionRename("forceConfigStyleOnOffset", "callSite.minSpan") // v3.8.2
case class ScalafmtOptimizer(
    dequeueOnNewStatements: Boolean = true,
    escapeInPathologicalCases: Boolean = true,
    maxVisitsPerToken: Int = 10000,
    maxDepth: Int = 100,
    acceptOptimalAtHints: Boolean = true,
    disableOptimizationsInsideSensitiveAreas: Boolean = true,
    pruneSlowStates: ScalafmtOptimizer.PruneSlowStates =
      ScalafmtOptimizer.PruneSlowStates.Yes,
    recurseOnBlocks: Boolean = true,
    callSite: ClauseElement = ClauseElement(minCount = 2, minSpan = 150),
    defnSite: ClauseElement = ClauseElement.disabled,
) {

  private[config] def conservative: ScalafmtOptimizer = {
    // The tests were not written in this style
    val cfg = ClauseElement(minSpan = 500, minCount = 5)
    copy(callSite = cfg, defnSite = cfg)
  }

}

object ScalafmtOptimizer {
  private[config] val default = ScalafmtOptimizer()

  implicit lazy val surface: generic.Surface[ScalafmtOptimizer] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[ScalafmtOptimizer] = generic
    .deriveCodecEx(default).noTypos.detectSectionRenames

  /** Parameters to optimize route search and greatly reduce search space on
    * argument clauses (all conditions must be met):
    *
    * @param minSpan
    *   Disabled if negative. Otherwise, requires that the non-whitespace byte
    *   offset between the opening parens and closing parens is greater than
    *   this value.
    * @param minCount
    *   Disabled if non-positive. Otherwise, requires that the clause has at
    *   least as many arguments/parameters.
    */
  case class ClauseElement(minSpan: Int = -1, minCount: Int = 0) {
    val isEnabled: Boolean = minSpan >= 0 && minCount > 0
  }

  private[config] object ClauseElement {
    val disabled = ClauseElement()
    implicit val surface: generic.Surface[ClauseElement] = generic
      .deriveSurface[ClauseElement]
    implicit val codec: ConfCodecEx[ClauseElement] = generic
      .deriveCodecEx(disabled).noTypos
  }

  sealed abstract class PruneSlowStates
  object PruneSlowStates {
    case object No extends PruneSlowStates
    case object Yes extends PruneSlowStates
    case object Only extends PruneSlowStates

    implicit val reader: ConfCodecEx[PruneSlowStates] = ReaderUtil
      .oneOfCustom[PruneSlowStates](No, Yes, Only) { case Conf.Bool(flag) =>
        Configured.Ok(if (flag) Yes else No)
      }
  }

}
