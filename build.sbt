triggeredMessage in ThisBuild := Watched.clearWhenTriggered

name := "scalafmt"

organization := "org.scalafmt"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

testOptions in Test += Tests.Argument("-oD")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

pomExtra := (
  <url>http://github.com/olafurpg/scalafmt</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:olafurpg/scalafmt.git</url>
      <connection>scm:git:git@github.com:olafurpg/scalafmt.git</connection>
    </scm>
    <developers>
      <developer>
        <id>olafurpg</id>
        <name>Ólafur Páll Geirsson</name>
        <url>http://geirsson.com</url>
      </developer>
    </developers>)

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.scalameta" %% "scalameta" % "0.1.0-SNAPSHOT",
  "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0" % "test",
  "com.lihaoyi" %% "scalatags" % "0.5.4" % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)


