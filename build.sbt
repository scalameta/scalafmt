name := "scalafmt"

organization := "scala.fmt"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "0.0.4",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc()
)

initialCommands := "import scala.fmt.scalafmt._"

