import Dependencies._

addCommandAlias("downloadIdea", "intellij/updateIdea")

lazy val buildSettings = Seq(
  organization := "com.geirsson",
  version := sys.props.getOrElse("scalafmt.version", version.value),
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala211, scala212),
  scalaCompilerBridgeSource :=
    ("org.scala-sbt" % "compiler-interface" % "0.13.15" % "component").sources,
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
commands += CiCommand("ci-sbt-scalafmt")(
  "scalafmtSbt/it:test" ::
    Nil
)
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
      metaconfig,
      scalameta,
      "com.typesafe" % "config" % "1.2.1",
      // Test dependencies
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0" % Test,
      "com.lihaoyi"                    %% "scalatags" % "0.6.3" % Test,
      scalametaTestkit                 % Test,
      scalatest                        % Test
    ),
    addCompilerPlugin(paradise)
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

def isOnly(scalaV: String) = Seq(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV)
)

lazy val scalafmtSbt = project
  .configs(IntegrationTest)
  .settings(
    allSettings,
    Defaults.itSettings,
    moduleName := "sbt-scalafmt",
    isOnly(scala212),
    sbtPlugin := true,
    sbtVersion in Global := "1.0.0-M5",
    test.in(IntegrationTest) := RunSbtCommand(
      Seq(
        s"wow $scala212",
        "publishLocal",
        """set sbtVersion in Global := "0.13.15" """,
        "such scalafmtSbtTest/scripted",
        """set sbtVersion in Global := "1.0.0-M5" """,
        s"wow $scala211"
      ).mkString("; ", "; ", "")
    )(state.value)
  )
  .dependsOn(cli)

lazy val scalafmtSbtTest = project
  .settings(
    allSettings,
    noPublish,
    isOnly(scala210),
    sbtPlugin := true,
    ScriptedPlugin.scriptedSettings,
    sbtVersion := "0.13.15",
    scriptedSbt := "0.13.15",
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
    isOnly(scala212),
    moduleName := "scalafmt-benchmarks",
    libraryDependencies ++= Seq(
      scalametaTestkit,
      scalatest % Test
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
  addCompilerPlugin(paradise),
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
def scala211 = "2.11.11"
def scala212 = "2.12.2"
def extraSbtBootOptions: Seq[String] = {
  // pass along custom boot properties if specified
  val bootProps = "sbt.boot.properties"
  sys.props.get(bootProps).map(x => s"-D$bootProps=$x").toList
}

lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings
