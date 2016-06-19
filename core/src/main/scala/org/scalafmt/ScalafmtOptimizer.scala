package org.scalafmt

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
  * @param MaxVisitsPerToken
  *                   Visit the same formatToken at most [[MaxVisitsPerToken]]
  *                   times.
  * @param MaxEscapes How often do we try to escape before giving up and
  *                   use original formatting.
  * @param MaxDepth   Maximum depth of recursion.
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
  */
case class ScalafmtOptimizer(dequeueOnNewStatements: Boolean,
                             escapeInPathologicalCases: Boolean,
                             MaxVisitsPerToken: Int,
                             MaxEscapes: Int,
                             MaxDepth: Int,
                             acceptOptimalAtHints: Boolean,
                             disableOptimizationsInsideSensitiveAreas: Boolean,
                             pruneSlowStates: Boolean,
                             recurseOnBlocks: Boolean)

object ScalafmtOptimizer {
  val default = ScalafmtOptimizer(
      dequeueOnNewStatements = true,
      escapeInPathologicalCases = true,
      MaxVisitsPerToken = 200,
      MaxEscapes = 16,
      MaxDepth = 100,
      acceptOptimalAtHints = true,
      disableOptimizationsInsideSensitiveAreas = true,
      pruneSlowStates = true,
      recurseOnBlocks = true
  )

  val noOptimizations = default.copy(
      dequeueOnNewStatements = false,
      escapeInPathologicalCases = false,
      acceptOptimalAtHints = false,
      disableOptimizationsInsideSensitiveAreas = false,
      pruneSlowStates = false,
      recurseOnBlocks = false
  )
}
