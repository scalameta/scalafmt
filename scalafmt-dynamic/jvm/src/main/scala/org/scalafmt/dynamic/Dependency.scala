package org.scalafmt.dynamic

case class Dependency(group: String, artifact: String, version: String)

object Dependency {

  def dependencies(version: ScalafmtVersion): Seq[Dependency] = {
    val scalaVersion = getScalaVersion(version)
    val scalaBinaryVersion = getScalaBinaryVersion(scalaVersion)
    val core = "scalafmt-core_" + scalaBinaryVersion
    Seq(
      Dependency(organization(version), core, version.toString),
      Dependency("org.scala-lang", "scala-reflect", scalaVersion),
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
