lazy val scalafmt = project.in(file("scalafmt")).settings(
  name := "scalafmt",
  organization := "org.scalafmt",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.7",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % "0.1.0-SNAPSHOT",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
  )
)

