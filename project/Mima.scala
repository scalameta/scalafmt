import sbt.Keys._
import sbt._

import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._

import sbtdynver.DynVerPlugin.autoImport.previousStableVersion

object Mima {

  val settings: Seq[Def.Setting[?]] = Seq(
    mimaPreviousArtifacts := previousStableVersion.value.map { v =>
      val m = organization.value % moduleName.value % v
      if (crossPaths.value) m.cross(CrossVersion.binary) else m
    }.toSet,
    mimaFiltersDirectory :=
      (ThisBuild / baseDirectory).value / moduleName.value / "mima-filters",
    mimaReportBinaryIssues / aggregate := false, // no fan out
  )

  // Exclude scalafmt-dynamic-core dependencies
  private val reflected = Set(
    "org.scalafmt.config.ScalafmtConfig",
    "org.scalafmt.config.ScalafmtConfig.intellij",
    "org.scalafmt.config.ScalafmtConfig.project",
    "org.scalafmt.config.ScalafmtConfig.indent",
    "org.scalafmt.config.ScalafmtConfig.forSbt",
    "org.scalafmt.config.ScalafmtConfig.withoutRewrites",
    "org.scalafmt.config.ScalafmtConfig.hasRewrites",
    "org.scalafmt.config.ScalafmtConfig.needGitAutoCRLF",
    "org.scalafmt.config.ScalafmtConfig.withGitAutoCRLF",
    "org.scalafmt.config.ProjectFiles",
    "org.scalafmt.config.ProjectFiles.matcher",
    "org.scalafmt.config.ProjectFiles.git",
    "org.scalafmt.config.ProjectFiles.FileMatcher",
    "org.scalafmt.config.ProjectFiles.FileMatcher.matches",
    "org.scalafmt.config.Indents",
    "org.scalafmt.config.Indents.main",
    "org.scalafmt.config.Indents.callSite",
    "org.scalafmt.config.Indents.defnSite",
  )

  private val internalPackages = Seq(
    "org.scalafmt.config.",
    "org.scalafmt.internal.",
    "org.scalafmt.rewrite.",
    "org.scalafmt.util.",
  )

  private val coreFilter: ProblemFilter = _.matchName.forall(fn =>
    !internalPackages.exists(fn.startsWith) ||
      reflected(fn.stripSuffix("$").replaceAll("[#$]", ".")),
  )

  val coreSettings: Seq[Def.Setting[?]] = Def
    .settings(settings, mimaBinaryIssueFilters += coreFilter)

}
