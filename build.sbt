triggeredMessage in ThisBuild := Watched.clearWhenTriggered

lazy val scalafmt = project.in(file("scalafmt")).settings(
  name := "scalafmt",
  organization := "org.scalafmt",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.7",
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  libraryDependencies ++= Seq(
    "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.scalameta" %% "scalameta" % "0.1.0-SNAPSHOT",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"
  )
)


