// The version number used in docs.
def latestStableVersion: String = "0.6.5"

addCommandAlias("downloadIdea", "intellij/updateIdea")

lazy val buildSettings = Seq(
  organization := "com.geirsson",
  version := "0.6.5",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.11.8", "2.12.1"),
  updateOptions := updateOptions.value.withCachedResolution(true)
)

lazy val noDocs = Seq(
  sources in (Compile, doc) := Nil
)

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  libraryDependencies += "org.scalameta" %% "scalameta" % Deps.scalameta,
  addCompilerPlugin(
    "org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise"
) ++ noDocs

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
  homepage := Some(url("https://github.com/scalameta/scalafmt")),
  autoAPIMappings := true,
  apiURL := Some(url("https://olafurpg.github.io/scalafmt/docs/")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/scalameta/scalafmt"),
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
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)

lazy val buildInfoSettings: Seq[Def.Setting[_]] = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    "nightly" -> version.value,
    "stable" -> latestStableVersion,
    "scala" -> scalaVersion.value,
    "coursier" -> Deps.coursier,
    scalaVersion,
    sbtVersion
  ),
  buildInfoPackage := "org.scalafmt",
  buildInfoObject := "Versions"
)

lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings

lazy val root = project
  .in(file("."))
  .settings(
    commands += Command.command("ci-fast") { s =>
      "clean" ::
        "very test" ::
        s
    },
    commands += Command.command("ci-slow") { s =>
      "core/test:runMain org.scalafmt.FormatExperimentApp" ::
        "; publishLocal ; scripted" ::
        s
    },
    allSettings,
    noPublish,
    initialCommands in console :=
      """
        |import scala.meta._
        |import org.scalafmt._
          """.stripMargin
  )
  .aggregate(
    benchmarks,
    bootstrap,
    cli,
    utils,
    testUtils,
    core,
    readme,
    scalafmtSbt,
    intellij
  )
  .dependsOn(core)

lazy val core = project
  .settings(
    allSettings,
    metaMacroSettings,
    buildInfoSettings,
    moduleName := "scalafmt-core",
    libraryDependencies ++= Seq(
      "com.geirsson"   %% "metaconfig-core" % "0.1.2",
      "com.lihaoyi"    %% "sourcecode"      % "0.1.2",
      "org.scalameta"  %% "scalameta"       % Deps.scalameta,
      "org.scala-lang" % "scala-reflect"    % scalaVersion.value,
      "com.typesafe"   % "config"           % "1.2.1",
      // Test dependencies
      "org.scala-lang"                 % "scala-compiler"  % scalaVersion.value % Test,
      "ch.qos.logback"                 % "logback-classic" % "1.1.6"            % Test,
      "com.googlecode.java-diff-utils" % "diffutils"       % "1.3.0"            % Test,
      "com.lihaoyi"                    %% "scalatags"      % "0.6.3"            % Test,
      "org.apache.commons"             % "commons-math3"   % "3.6"              % Test,
      "org.scalatest"                  %% "scalatest"      % Deps.scalatest     % Test
    ),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .dependsOn(utils, testUtils % "test->compile")
  .enablePlugins(BuildInfoPlugin)

lazy val cliJvmOptions = Seq(
  "-Xss4m"
)

lazy val cli = project
  .settings(
    allSettings,
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

lazy val is210 = Seq(
  scalaVersion := "2.10.6",
  crossScalaVersions := Seq("2.10.6")
)

lazy val bootstrap = project
  .settings(
    allSettings,
    buildInfoSettings,
    is210,
    moduleName := "scalafmt-bootstrap",
    libraryDependencies ++= Seq(
      "com.martiansoftware" % "nailgun-server"  % "0.9.1",
      "io.get-coursier"     %% "coursier"       % Deps.coursier,
      "io.get-coursier"     %% "coursier-cache" % Deps.coursier,
      "org.scalatest"       %% "scalatest"      % Deps.scalatest % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val scalafmtSbt = project
  .settings(
    allSettings,
    ScriptedPlugin.scriptedSettings,
    scripted := scripted
      .dependsOn(
        publishLocal in cli,
        publishLocal in core,
        publishLocal in bootstrap,
        publishLocal in utils
      )
      .evaluated,
    sbtPlugin := true,
    is210,
    // In convention of sbt plugins, the module is sbt-scalafmt instead of scalafmt-sbt.
    moduleName := "sbt-scalafmt",
    scriptedLaunchOpts := Seq(
      "-Dplugin.version=" + version.value,
      "-Dscalafmt.scripted=true",
      // .jvmopts is ignored, simulate here
      "-XX:MaxPermSize=256m",
      "-Xmx2g",
      "-Xss2m"
    ),
    scriptedBufferLog := false
  )
  .dependsOn(bootstrap)

lazy val intellij = project
  .settings(
    allSettings,
    buildInfoSettings,
    noPublish,
    noDocs,
    ideaBuild := "2016.3.2",
    test := {}, // no need to download IDEA to run all tests.
    ideaEdition := IdeaEdition.Community,
    ideaDownloadDirectory in ThisBuild := baseDirectory.value / "idea",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    cleanFiles += ideaDownloadDirectory.value
  )
  .dependsOn(core, cli)
  .enablePlugins(SbtIdeaPlugin)

lazy val benchmarks = project
  .settings(
    allSettings,
    noPublish,
    crossScalaVersions := Seq("2.11.8"),
    moduleName := "scalafmt-benchmarks",
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
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(testUtils)
  .enablePlugins(JmhPlugin)

lazy val readme = scalatex
  .ScalatexReadme(projectId = "readme",
                  wd = file(""),
                  url = "https://github.com/scalameta/scalafmt/tree/master",
                  source = "Readme")
  .settings(
    allSettings,
    noPublish,
    test := {
      run.in(Compile).toTask(" --validate-links").value
    },
    libraryDependencies ++= Seq(
      "com.twitter" %% "util-eval" % "6.41.0"
    )
  )
  .dependsOn(
    core,
    cli
  )

// General utilities that shared between bootstrap and core.
lazy val utils = project.settings(
  allSettings,
  moduleName := "scalafmt-utils",
  libraryDependencies ++= Seq(
    "com.typesafe" % "config" % "1.2.1"
  )
)

lazy val testUtils = project
  .settings(
    allSettings,
    noPublish,
    moduleName := "scalafmt-test-utils",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-io"  % "1.3.2",
      "org.rauschig"       % "jarchivelib" % "0.7.1"
    )
  )
  .dependsOn(utils)
