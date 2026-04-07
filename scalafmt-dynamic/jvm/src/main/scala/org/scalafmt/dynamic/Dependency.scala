package org.scalafmt.dynamic

import org.scalafmt.interfaces.RepositoryPackage

private[dynamic] object Dependency {

  def dependencies(version: ScalafmtVersion): (String, Seq[RepositoryPackage]) = {
    val scalaVersion = getScalaVersion(version)
    val scalaBinaryVersion = getScalaBinaryVersion(scalaVersion)
    val core = "scalafmt-core_" + scalaBinaryVersion
    scalaVersion -> Seq(
      new RepositoryPackage(organization(version), core, version.toString),
      new RepositoryPackage("org.scala-lang", "scala-reflect", scalaVersion),
    )
  }

  @inline
  private def getScalaBinaryVersion(scalaVersion: String): String = scalaVersion
    .substring(0, scalaVersion.lastIndexOf('.'))

  @inline
  private def getScalaVersion(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(0, 7, 0)) "2.11.12"
    else if (version < ScalafmtVersion(2, 1, 2)) BuildInfo.scala212
    else BuildInfo.scala

  @inline
  private def organization(version: ScalafmtVersion): String =
    if (version < ScalafmtVersion(2, 0, 0, 2)) "com.geirsson"
    else "org.scalameta"

}
