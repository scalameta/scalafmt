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
  *     the same point later, the first solution is preferred if it can be
  *     verified to be always better (see
  *     [[org.scalafmt.internal.State.alwaysBetter]]).
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
case class ScalafmtOptimizer(
    dequeueOnNewStatements: Boolean = true,
    escapeInPathologicalCases: Boolean = true,
    maxVisitsPerToken: Int = 10000,
    maxDepth: Int = 100,
    acceptOptimalAtHints: Boolean = true,
    disableOptimizationsInsideSensitiveAreas: Boolean = true,
    pruneSlowStates: Boolean = true,
    recurseOnBlocks: Boolean = true,
    @annotation.ExtraName("forceConfigStyleOnOffset")
    forceConfigStyleMinSpan: Int = 150,
    forceConfigStyleMinArgCount: Int = 2,
) {
  def getCallSite: ClauseElement = ClauseElement(
    minSpan = forceConfigStyleMinSpan,
    minCount = forceConfigStyleMinArgCount,
  )

  private[config] def conservative: ScalafmtOptimizer =
    // The tests were not written in this style
    copy(forceConfigStyleMinSpan = 500, forceConfigStyleMinArgCount = 5)

}

object ScalafmtOptimizer {
  private[config] val default = ScalafmtOptimizer()

  implicit lazy val surface: generic.Surface[ScalafmtOptimizer] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[ScalafmtOptimizer] = generic
    .deriveCodecEx(default).noTypos

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

}
