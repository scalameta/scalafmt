lazy val buildSettings = Seq(
  organization := "org.scalafmt",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.7",
  updateOptions := updateOptions.value.withCachedResolution(true),
  // Many useful rules are ignored, at least they're explicitly ignored.
  wartremoverWarnings in (Compile, compile) ++=
    Warts.allBut(
      // TODO(olafur) include these below.
      Wart.Nothing, // Can't provide explicit type for scala.meta.Tree.collect.
      Wart.ToString, // Issues in logger, solvable with Loggable typeclass.
      Wart.Any, // Issues in logger with format strings.

      Wart.Throw,
      Wart.NoNeedForMonad,
      Wart.FinalCaseClass,
      Wart.NonUnitStatements,
      Wart.MutableDataStructures,
      Wart.IsInstanceOf,
      Wart.Var,
      Wart.Null,
      Wart.DefaultArguments)
)

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Xlint"
)

lazy val commonSettings = Seq(
  triggeredMessage in ThisBuild := Watched.clearWhenTriggered,
  scalacOptions in (Compile, console) := compilerOptions :+ "-Yrepl-class-based",
  testOptions in Test += Tests.Argument("-oD")
)

// Haven't published yet.
lazy val publishSettings = Seq(
  pomExtra :=
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
      </developers>
)

lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings


lazy val root = project.in(file("."))
  .settings(moduleName := "scalafmt")
  .settings(allSettings)
  .settings(
    initialCommands in console :=
      """
        |import io.finch.{Endpoint => _, _}
        |import io.finch.argonaut._
        |import io.finch.request._
        |import io.finch.request.items._
        |import io.finch.response._
        |import io.finch.route._
      """.stripMargin
  )
  .aggregate(core)
  .dependsOn(core)

lazy val core = project.in(file("core"))
  .settings(allSettings)
  .settings(
    moduleName := "scalafmt-core",
    mainClass in assembly := Some("org.scalafmt.Cli"),
    assemblyJarName in assembly := "scalafmt.jar",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.3.0",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
      "ch.qos.logback" % "logback-classic" % "1.1.3",
      "org.scalameta" %% "scalameta" % "0.0.5-M1",
      "com.lihaoyi" %% "sourcecode" % "0.1.0",
      "com.ibm" %% "couchdb-scala" % "0.6.0" % "test",
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0" % "test",
      "com.lihaoyi" %% "scalatags" % "0.5.4" % "test",
      "org.apache.commons" % "commons-math3" % "3.6" % "test",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test"
    )
  ).settings(allSettings)

