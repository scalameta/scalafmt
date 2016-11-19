import scoverage.ScoverageSbtPlugin.ScoverageKeys.coverageHighlighting

lazy val buildSettings = Seq(
  organization := "com.geirsson",
  // See core/src/main/scala/org/scalafmt/Versions.scala
  version := org.scalafmt.Versions.nightly,
  scalaVersion := org.scalafmt.Versions.scala,
  updateOptions := updateOptions.value.withCachedResolution(true)
)

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  libraryDependencies += "org.scalameta" %% "scalameta" % Deps.scalameta,
  sources in (Compile, doc) := Nil,
  addCompilerPlugin(
    "org.scalameta" % "paradise" % "3.0.0-M5" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise"
)

lazy val compilerOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
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
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  licenses := Seq(
    "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
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

lazy val credentialSettings = Seq(
  credentials += Credentials("Sonatype Nexus Repository Manager",
                             "oss.sonatype.org",
                             "olafurpg@gmail.com",
                             sonatypePassword)
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {}
)

lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings

lazy val root = project
  .in(file("."))
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
  )
  .aggregate(
    benchmarks,
    bootstrap,
    cli,
    utils,
    core,
    metaconfig,
    readme
  )
  .dependsOn(core)

lazy val core = project
  .settings(allSettings)
  .settings(
    metaMacroSettings,
    moduleName := "scalafmt-core",
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %% "sourcecode"   % "0.1.2",
      "org.scalameta"  %% "scalameta"    % Deps.scalameta,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.typesafe"   % "config"        % "1.2.1",
      // Test dependencies
      "org.scalariform"                %% "scalariform"    % Deps.scalariform   % Test,
      "org.scala-lang"                 % "scala-compiler"  % scalaVersion.value % Test,
      "ch.qos.logback"                 % "logback-classic" % "1.1.6"            % Test,
      "com.googlecode.java-diff-utils" % "diffutils"       % "1.3.0"            % Test,
      "com.lihaoyi"                    %% "scalatags"      % "0.5.4"            % Test,
      "org.apache.commons"             % "commons-math3"   % "3.6"              % Test,
      "org.scalatest"                  %% "scalatest"      % Deps.scalatest     % Test
    ),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .dependsOn(metaconfig, utils)

lazy val cliJvmOptions = Seq(
  "-Xss4m"
)

lazy val cli = project
  .settings(allSettings)
  .settings(
    metaMacroSettings,
    packSettings,
    packMain := Map("scalafmt_pack" -> "org.scalafmt.cli.Cli"),
    packJvmOpts := Map(
      "scalafmt_pack" -> cliJvmOptions
    ),
    moduleName := "scalafmt-cli",
    mainClass in assembly := Some("org.scalafmt.cli.Cli"),
    libraryDependencies ++= Seq(
      "com.martiansoftware" % "nailgun-server" % "0.9.1",
      "com.github.scopt"    %% "scopt"         % "3.5.0"
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val bootstrap = project
  .settings(
    allSettings,
    //  crossScalaVersions := Seq("2.10.6", "2.11.8"),
    moduleName := "scalafmt-bootstrap",
    sources in Compile +=
      baseDirectory.value / "../core/src/main/scala/org/scalafmt/Versions.scala",
    libraryDependencies ++= Seq(
      "com.martiansoftware" % "nailgun-server"  % "0.9.1",
      "io.get-coursier"     %% "coursier"       % "1.0.0-M14",
      "io.get-coursier"     %% "coursier-cache" % "1.0.0-M14",
      "org.scalatest"       %% "scalatest"      % Deps.scalatest % Test
    )
  )
  .dependsOn(utils)

lazy val benchmarks = project
  .settings(moduleName := "scalafmt-benchmarks")
  .settings(allSettings)
  .settings(noPublish)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalariform" %% "scalariform" % Deps.scalariform,
      "org.scalatest"   %% "scalatest"   % Deps.scalatest % Test
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
  )
  .dependsOn(core % "compile->test")
  .enablePlugins(JmhPlugin)

lazy val readme = scalatex
  .ScalatexReadme(projectId = "readme",
                  wd = file(""),
                  url = "https://github.com/olafurpg/scalafmt/tree/master",
                  source = "Readme")
  .settings(allSettings)
  .settings(noPublish)
  .dependsOn(core)
  .dependsOn(cli)
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "util-eval" % "6.34.0"
    ),
    dependencyOverrides += "com.lihaoyi" %% "scalaparse" % "0.3.1"
  )

lazy val sonatypePassword = sys.env.getOrElse("SONATYPE_PW", "")

// General utilities that shared between bootstrap and core.
lazy val utils = project.settings(
  allSettings,
  moduleName := "scalafmt-utils",
  libraryDependencies ++= Seq(
    "com.typesafe" % "config" % "1.2.1"
  )
)

lazy val metaconfig = project.settings(
  allSettings,
  metaMacroSettings,
  publishSettings,
  moduleName := "metaconfig",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % Deps.scalatest % Test
  )
)
