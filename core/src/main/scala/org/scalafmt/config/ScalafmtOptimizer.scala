package org.scalafmt.config

import metaconfig.ConfigReader

/**
  * Configuration for scalafmt optimizations.
  *
  * @param dequeueOnNewStatements
  *                   Clear the search queue on new statements.
  * @param escapeInPathologicalCases
  *                   Use heuristics to escape when the search state grows
  *                   out of bounds.
  *
  *                   An optimization that trades off optimal formatting
  *                   output in order to complete in a reasonable time.
  *                   Used as a last resort.
  * @param maxVisitsPerToken
  *                   Visit the same formatToken at most [[maxVisitsPerToken]]
  *                   times.
  * @param maxEscapes How often do we try to escape before giving up and
  *                   use original formatting.
  * @param maxDepth   Maximum depth of recursion.
  * @param acceptOptimalAtHints
  *                   Whether to listen to optimalAt fields in Splits.
  * @param disableOptimizationsInsideSensitiveAreas
  *                   Do not optimize inside certain areas such as term apply.
  * @param pruneSlowStates
  *                   Eliminate solutions that move slower than other solutions.
  *
  *                   If a solution reaches a point X first and other
  *                   solution that reaches the same point later, the first
  *                   solution is preferred if it can be verified to be
  *                   always better (see
  *                   [[org.scalafmt.internal.State.alwaysBetter()]]).
  *
  *                   Note. This affects the output positively because it
  *                   breaks a tie between two equally expensive solutions
  *                   by eliminating the slower one.
  *
  *                   Example, solution 1 is preferred even though both
  *                   solutions cost the same:
  *
  *                   // solution 1
  *                   a + b +
  *                   c + d
  *                   // solution 2
  *                   a +
  *                   b + c + d
  * @param recurseOnBlocks
  *                   Recursively format { ... } blocks inside no
  *                   optimization zones.
  *
  *                   By starting a new search queue, we can perform
  *                   aggressive optimizations inside optimizations zones.
  * @param forceConfigStyleOnOffset
  *   If negative number, does nothing. If n >= 0, then scalafmt will force
  *   "config style" on Term.Apply nodes IF it has more than [[forceConfigStyleOnArgCount]]
  *   arguments AND the non-whitespace byte offset between the opening
  *   parens and closing parens is greater than [[forceConfigStyleOnOffset]].
  *   By forcing config style on such applications, the search space is greatly
  *   reduced.
  */
@ConfigReader
case class ScalafmtOptimizer(
    dequeueOnNewStatements: Boolean = true,
    escapeInPathologicalCases: Boolean = true,
    maxVisitsPerToken: Int = 513,
    maxEscapes: Int = 16,
    maxDepth: Int = 100,
    acceptOptimalAtHints: Boolean = true,
    disableOptimizationsInsideSensitiveAreas: Boolean = true,
    pruneSlowStates: Boolean = true,
    recurseOnBlocks: Boolean = true,
    forceConfigStyleOnOffset: Int = 500,
    forceConfigStyleOnArgCount: Int = 5
)

object ScalafmtOptimizer {
  val default = ScalafmtOptimizer()

  // TODO(olafur) uncomment once scala.meta converter supports default args.
//  val noOptimizations = default.copy(
//    dequeueOnNewStatements = false,
//    escapeInPathologicalCases = false,
//    acceptOptimalAtHints = false,
//    disableOptimizationsInsideSensitiveAreas = false,
//    pruneSlowStates = false,
//    recurseOnBlocks = false
//  )
}
