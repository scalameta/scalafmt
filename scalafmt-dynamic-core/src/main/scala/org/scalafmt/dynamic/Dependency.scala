package org.scalafmt.dynamic

import org.scalafmt.interfaces.RepositoryPackage

private[dynamic] object Dependency {

  def dependencies(version: ScalafmtVersion): (String, Seq[RepositoryPackage]) = {
    val scalaVersion = getScalaVersion(version)
    val scalaBinaryVersion = getScalaBinaryVersion(scalaVersion)
    val core = "scalafmt-core_" + scalaBinaryVersion
    scalaVersion -> {
      val coreDep =
        new RepositoryPackage(organization(version), core, version.toString)
      if (scalaBinaryVersion == "3") Seq(coreDep)
      else Seq(
        coreDep,
        new RepositoryPackage("org.scala-lang", "scala-reflect", scalaVersion),
      )
    }
  }

  @inline
  private def getScalaBinaryVersion(scalaVersion: String): String =
    if (scalaVersion.startsWith("2.")) scalaVersion
      .substring(0, scalaVersion.lastIndexOf('.'))
    else scalaVersion.substring(0, scalaVersion.indexOf('.'))

  @inline
  private def getScalaVersion(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(0, 7, 0)) "2.11.12"
    else if (version < ScalafmtVersion(2, 1, 2)) BuildInfo.scala212
    else if (
      version < ScalafmtVersion(3, 11, 1) && BuildInfo.scala.startsWith("3.")
    ) BuildInfo.scala213
    else BuildInfo.scala

  @inline
  private def organization(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(2, 0, 0, 2)) "com.geirsson"
    else "org.scalameta"

}
