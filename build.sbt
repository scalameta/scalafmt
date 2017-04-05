import Dependencies._

addCommandAlias("downloadIdea", "intellij/updateIdea")

lazy val buildSettings = Seq(
  organization := "com.geirsson",
  version := sys.props.getOrElse("scalafmt.version", version.value),
  scalaVersion := scala211,
  crossScalaVersions := Seq(scala211, "2.12.1"),
  updateOptions := updateOptions.value.withCachedResolution(true)
)

name := "scalafmtRoot"
allSettings
noPublish
commands += CiCommand("ci-fast")(
  "test" ::
    Nil
)
commands += CiCommand("ci-slow")(
  "core/test:runMain org.scalafmt.ScalafmtProps" ::
    Nil
)
commands += Command.command("ci-sbt-scalafmt") { s =>
  "very publishLocal" ::
    s"plz $scala210 scalafmtSbt/scripted" ::
    s
}
commands += CiCommand("ci-publish")(
  if (sys.env.contains("CI_PUBLISH")) s"publish" :: Nil
  else Nil
)

lazy val core = project
  .settings(
    allSettings,
    metaMacroSettings,
    buildInfoSettings,
    fork.in(run).in(Test) := true,
    moduleName := "scalafmt-core",
    libraryDependencies ++= Seq(
      "com.geirsson" %% "metaconfig-core" % "0.1.2",
      scalameta,
      "com.typesafe" % "config" % "1.2.1",
      // Test dependencies
      scalametaTestkit,
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0" % Test,
      "com.lihaoyi"                    %% "scalatags" % "0.6.3" % Test,
      scalametaTestkit                 % Test,
      scalatest                        % Test
    ),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
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
  publishMavenStyle := false, // necessary for sbt community repo.
  scalaVersion := scala210,
  crossScalaVersions := Seq(scala210)
)

lazy val bootstrap = project
  .settings(
    allSettings,
    buildInfoSettings,
    is210,
    moduleName := "scalafmt-bootstrap",
    libraryDependencies ++= Seq(
      "com.martiansoftware" % "nailgun-server" % "0.9.1",
      "io.get-coursier"     %% "coursier" % coursier,
      "io.get-coursier"     %% "coursier-cache" % coursier,
      scalatest             % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val scalafmtSbt = project
  .settings(
    allSettings,
    ScriptedPlugin.scriptedSettings,
    sbtPlugin := true,
    test := {
      RunSbtCommand(
        s"; plz $scala211 publishLocal " +
          s"; plz $scala210 publishLocal " +
          s"; such scalafmtSbt/scripted"
      )(state.value)
    },
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
    ) ++ {
      // pass along custom boot properties if specified
      val bootProps = "sbt.boot.properties"
      sys.props.get(bootProps).map(x => s"-D$bootProps=$x").toList
    },
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
    crossScalaVersions := Seq(scala211),
    moduleName := "scalafmt-benchmarks",
    libraryDependencies ++= Seq(
      "org.scalariform" %% "scalariform" % scalariform,
      scalatest         % Test,
      scalametaTestkit  % Test
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
  .dependsOn(core)
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

lazy val ciScalaVersion = sys.env("CI_SCALA_VERSION")
def CiCommand(name: String)(commands: List[String]): Command =
  Command.command(name) { initState =>
    commands.foldLeft(initState) {
      case (state, command) => ci(command) :: state
    }
  }
def ci(command: String) = s"plz ${sys.env("CI_SCALA_VERSION")} $command"

def shouldPublishToBintray: Boolean = {
  if (!new File(sys.props("user.home") + "/.bintray/.credentials").exists)
    false
  else if (sys.props("publish.sonatype") != null) false
  else if (sys.env.contains("CI_PULL_REQUEST")) false
  else true
}

lazy val noDocs = Seq(
  sources in (Compile, doc) := Nil
)

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  libraryDependencies += scalameta,
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
  bintrayRepository := "maven",
  bintrayOrganization := Some("scalameta"),
  publishTo := {
    if (shouldPublishToBintray) publishTo.in(bintray).value
    else {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
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
    "stable" -> version.value.replaceAll("\\+.*", ""),
    "scala" -> scalaVersion.value,
    "coursier" -> coursier,
    scalaVersion,
    sbtVersion
  ),
  buildInfoPackage := "org.scalafmt",
  buildInfoObject := "Versions"
)

def scala210 = "2.10.6"
def scala211 = "2.11.8"

lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings
