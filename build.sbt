import scoverage.ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting

lazy val buildSettings = Seq(
  organization := "com.geirsson",
  // See core/src/main/scala/org/scalafmt/Versions.scala
  version :=  org.scalafmt.Versions.nightly,
  scalaVersion :=  org.scalafmt.Versions.scala,
  updateOptions := updateOptions.value.withCachedResolution(true),
  // Many useful rules are ignored, at least they're explicitly ignored.
  wartremoverWarnings in (Compile, compile) ++=
    Warts.allBut(
      // TODO(olafur) include these below.
      Wart.Nothing, // Can't provide explicit type for scala.meta.Tree.collect.
      Wart.ToString, // Issues in logger, solvable with Loggable typeclass.
      Wart.Any, // Issues in logger with format strings.
      Wart.AsInstanceOf, // pops up in pattern matching, why? It's guarded.

      // TODO(olafur) remove after https://github.com/puffnfresh/wartremover/issues/188
      Wart.ExplicitImplicitTypes,

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
  assemblyJarName in assembly := "scalafmt.jar",
  ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages :=
      ".*Debug;.*ScalaFmtLogger;.*Versions",
  testOptions in Test += Tests.Argument("-oD")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishMavenStyle := true,
  publishArtifact := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/olafurpg/scalafmt")),
  autoAPIMappings := true,
  apiURL := Some(url("https://olafurpg.github.io/scalafmt/docs/")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/olafurpg/scalafmt"),
      "scm:git:git@github.com:olafurpg/scalafmt.git"
    )
  ),
  pomExtra :=
    <developers>
      <developer>
        <id>olafurpg</id>
        <name>Ólafur Páll Geirsson</name>
        <url>https://geirsson.com</url>
      </developer>
    </developers>
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {}
)

lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings


lazy val root = project.in(file("."))
  .settings(moduleName := "scalafmt")
  .settings(allSettings)
  .settings(noPublish)
  .settings(
    initialCommands in console :=
      """
        |import scala.meta._
        |import org.scalafmt.internal._
        |import org.scalafmt._
      """.stripMargin
  ).aggregate(core, cli, benchmarks, scalafmtSbt, macros, readme)
  .dependsOn(core)


lazy val core = project
  .settings(allSettings)
  .settings(
    moduleName := "scalafmt-core",
    test in assembly := {
      (test in Test).value
      // TODO(olafur) This should be an integration test.
      (runMain in Test).toTask(" org.scalafmt.FormatExperimentApp").value
    },
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "sourcecode" % "0.1.1",
      "org.scalameta" %% "scalameta" % Deps.scalameta,

      // Test dependencies
      "org.scalariform" %% "scalariform" % Deps.scalariform ,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "test",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test",
      "ch.qos.logback" % "logback-classic" % "1.1.6" % "test",
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0" % "test",
      "com.lihaoyi" %% "scalatags" % "0.5.4" % "test",
      "org.apache.commons" % "commons-math3" % "3.6" % "test",
      "org.scalatest" %% "scalatest" % Deps.scalatest % "test"
    )
  )


lazy val macros = project
  .settings(allSettings)
  .settings(
    moduleName := "scalafmt-macros",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
  )

lazy val cli = project
    .settings(allSettings)
    .settings(
      moduleName := "scalafmt-cli",
      mainClass in assembly := Some("org.scalafmt.cli.Cli"),
      libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "3.3.0"
      )
    ).dependsOn(core % "compile->compile;test->test")
    .dependsOn(macros)


lazy val scalafmtSbt = project
  .settings(allSettings)
  .settings(ScriptedPlugin.scriptedSettings)
  .settings(
    coverageHighlighting := false,
    sbtPlugin := true,
    scalaVersion := "2.10.5",
    // In convention of sbt plugins, the module is sbt-scalafmt instead of scalafmt-sbt.
    moduleName := "sbt-scalafmt",
    sources in Compile +=
      baseDirectory.value / "../core/src/main/scala/org/scalafmt/Versions.scala",
    scriptedLaunchOpts := Seq(
      "-Dplugin.version=" + version.value,
      // .jvmopts is ignored, simulate here
      "-XX:MaxPermSize=256m", "-Xmx2g", "-Xss2m"
    ),
    scriptedBufferLog := false
  )

lazy val benchmarks = project
  .settings(moduleName := "scalafmt-benchmarks")
  .settings(allSettings)
  .settings(noPublish)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalariform" %% "scalariform" % Deps.scalariform,
      "org.scalatest" %% "scalatest" % Deps.scalatest % "test"
    ),
    javaOptions in run ++= Seq(
      "-Djava.net.preferIPv4Stack=true",
      "-XX:+AggressiveOpts",
      "-XX:+UseParNewGC",
      "-XX:+UseConcMarkSweepGC",
      "-XX:+CMSParallelRemarkEnabled",
      "-XX:+CMSClassUnloadingEnabled",
      "-XX:ReservedCodeCacheSize=128m",
      "-XX:MaxPermSize=1024m",
      "-Xss8M",
      "-Xms512M",
      "-XX:SurvivorRatio=128",
      "-XX:MaxTenuringThreshold=0",
      "-Xss8M",
      "-Xms512M",
      "-Xmx2G",
      "-server"
    )
  ).dependsOn(core % "compile->test")
  .enablePlugins(JmhPlugin)

lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/olafurpg/scalafmt/tree/master",
  source = "Readme")
    .settings(allSettings)
    .settings(noPublish)
    .dependsOn(core)
    .dependsOn(cli)
    .dependsOn(macros)
    .settings(
      libraryDependencies ++= Seq(
        "com.twitter" %% "util-eval" % "6.34.0"
      ),
      dependencyOverrides += "com.lihaoyi" %% "scalaparse" % "0.3.1"
    )

