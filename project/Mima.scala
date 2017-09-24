import com.typesafe.tools.mima.core._

object Mima {
  val ignoredABIProblems: Seq[ProblemFilter] = {
    // After v0.5, start running mima checks in CI and document breaking changes here.
    // See https://github.com/typesafehub/migration-manager/wiki/sbt-plugin#basic-usage
    Seq(
      // Essentially, only org.scalafmt.Scalafmt is protected by bincompat guarantees.
      ProblemFilters.exclude[Problem]("org.scalafmt.cli.*"),
      ProblemFilters.exclude[Problem]("org.scalafmt.config.*"),
      ProblemFilters.exclude[Problem]("org.scalafmt.internal.*"),
      ProblemFilters.exclude[Problem]("org.scalafmt.rewrite.*"),
      ProblemFilters.exclude[Problem]("org.scalafmt.util.*")
    )
  }
}
