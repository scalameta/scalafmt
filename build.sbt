import Dependencies._
import org.scalajs.sbtplugin.cross.CrossProject

version.in(ThisBuild) ~= { old =>
  sys.props.getOrElse("scalafmt.version", old.replace('+', '-'))
}

lazy val buildSettings = Seq(
  organization := "com.geirsson",
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala211, scala212),
  updateOptions := updateOptions.value.withCachedResolution(true),
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies += scalatest.value % Test,
  triggeredMessage in ThisBuild := Watched.clearWhenTriggered,
  scalacOptions in (Compile, console) := compilerOptions :+ "-Yrepl-class-based",
  scalacOptions in (Compile, console) --= Seq("-Xlint", "-Ywarn-dead-code"),
  assemblyJarName in assembly := "scalafmt.jar",
  testOptions in Test += Tests.Argument("-oD")
)

lazy val allSettings = buildSettings ++ publishSettings

name := "scalafmtRoot"
allSettings
noPublish
commands ++= ciCommands
addCommandAlias("downloadIdea", "intellij/updateIdea")

lazy val core = crossProject
  .in(file("scalafmt-core"))
  .settings(
    moduleName := "scalafmt-core",
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    allSettings,
    buildInfoSettings,
    fork.in(run).in(Test) := true,
    libraryDependencies ++= Seq(
      metaconfig.value,
      scalameta.value
    )
  )
  .jsSettings(
    libraryDependencies += metaconfigHocon.value
  )
  .jvmSettings(
    libraryDependencies += metaconfigTypesafe.value
  )
  .enablePlugins(BuildInfoPlugin)
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val cli = project
  .in(file("scalafmt-cli"))
  .settings(
    moduleName := "scalafmt-cli",
    allSettings,
    mainClass in assembly := Some("org.scalafmt.cli.Cli"),
    libraryDependencies ++= Seq(
      "com.martiansoftware" % "nailgun-server" % "0.9.1",
      "com.github.scopt" %% "scopt" % "3.5.0"
    )
  )
  .dependsOn(coreJVM)

def isOnly(scalaV: String) = Seq(
  scalaVersion := scalaV,
  crossScalaVersions := Seq(scalaV)
)

lazy val `scalafmt-cli-sbt` = project
  .configs(IntegrationTest)
  .settings(
    allSettings,
    Defaults.itSettings,
    mimaPreviousArtifacts := Set.empty,
    moduleName := "sbt-cli-scalafmt",
    isOnly(scala212),
    sbtPlugin := true,
    sbtVersion in Global := "1.0.0",
    test.in(IntegrationTest) := RunSbtCommand(
      Seq(
        s"plz $scala212 publishLocal",
        """set sbtVersion in Global := "0.13.16" """,
        "such scalafmt-sbt-tests/scripted",
        """set sbtVersion in Global := "1.0.0" """
      ).mkString("; ", "; ", "")
    )(state.value)
  )
  .dependsOn(cli)

lazy val `scalafmt-sbt` = project
  .settings(
    allSettings,
    mimaPreviousArtifacts := Set.empty,
    moduleName := "sbt-scalafmt",
    isOnly(scala212),
    sbtPlugin := true,
    sbtVersion in Global := "1.0.0"
  )
  .dependsOn(coreJVM)

lazy val `scalafmt-sbt-tests` = project
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
  .in(file("scalafmt-intellij"))
  .settings(
    allSettings,
    buildInfoSettings,
    noPublish,
    noDocs,
    mimaReportBinaryIssues := {},
    ideaBuild := "2016.3.2",
    test := {}, // no need to download IDEA to run all tests.
    ideaEdition := IdeaEdition.Community,
    ideaDownloadDirectory in ThisBuild := baseDirectory.value / "idea",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    cleanFiles += ideaDownloadDirectory.value
  )
  .dependsOn(coreJVM, cli)
  .enablePlugins(SbtIdeaPlugin)

lazy val tests = project
  .in(file("scalafmt-tests"))
  .settings(
    allSettings,
    noPublish,
    libraryDependencies ++= Seq(
      // Test dependencies
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
      "com.lihaoyi" %% "scalatags" % "0.6.3",
      scalametaTestkit
    )
  )
  .dependsOn(
    cli
  )

lazy val benchmarks = project
  .in(file("scalafmt-benchmarks"))
  .settings(
    allSettings,
    noPublish,
    isOnly(scala212),
    moduleName := "scalafmt-benchmarks",
    libraryDependencies ++= Seq(
      scalametaTestkit,
      scalatest.value % Test
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
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)

lazy val readme = scalatex
  .ScalatexReadme(
    projectId = "readme",
    wd = file(""),
    url = "https://github.com/scalameta/scalafmt/tree/master",
    source = "Readme")
  .settings(
    git.remoteRepo := "git@github.com:scalameta/scalafmt.git",
    siteSourceDirectory := target.value / "scalatex",
    allSettings,
    noPublish,
    publish := {
      ghpagesPushSite
        .dependsOn(run.in(Compile).toTask(" --validate-links"))
        .value
    },
    test := {
      run.in(Compile).toTask(" --validate-links").value
    },
    libraryDependencies ++= Seq(
      "com.twitter" %% "util-eval" % "6.41.0",
      "com.geirsson" %% "metaconfig-docs" % metaconfigV
    )
  )
  .enablePlugins(GhpagesPlugin)
  .dependsOn(
    coreJVM,
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
  mimaPreviousArtifacts := Set(
    organization.value % s"${moduleName.value}_${scalaBinaryVersion.value}" % "1.0.0"
  ),
  mimaBinaryIssueFilters ++= Mima.ignoredABIProblems,
  licenses := Seq(
    "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/scalameta/scalafmt")),
  autoAPIMappings := true,
  apiURL := Some(url("https://olafurpg.github.io/scalafmt/docs/")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/scalameta/scalafmt"),
      "scm:git:git@github.com:scalameta/scalafmt.git"
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
  mimaPreviousArtifacts := Set.empty,
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)

lazy val stableVersion = Def.setting(version.value.replaceAll("\\-.*", ""))
lazy val buildInfoSettings: Seq[Def.Setting[_]] = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    "scalameta" -> scalametaV,
    "nightly" -> version.value,
    "stable" -> stableVersion.value,
    "scala" -> scalaVersion.value,
    "coursier" -> coursier,
    "commit" -> Seq("git", "rev-parse", "HEAD").!!.trim,
    "timestamp" -> System.currentTimeMillis().toString,
    scalaVersion,
    sbtVersion
  ),
  buildInfoPackage := "org.scalafmt",
  buildInfoObject := "Versions"
)

def currentCommit = {

}
def scala210 = "2.10.6"
def scala211 = "2.11.11"
def scala212 = "2.12.2"
def extraSbtBootOptions: Seq[String] = {
  // pass along custom boot properties if specified
  val bootProps = "sbt.boot.properties"
  sys.props.get(bootProps).map(x => s"-D$bootProps=$x").toList
}

def ciCommands = Seq(
  CiCommand("ci-fast")(
    "test" ::
      Nil
  ),
  CiCommand("ci-slow")(
    "tests/test:runMain org.scalafmt.ScalafmtProps" ::
      Nil
  ),
  Command.command("ci-sbt-scalafmt") { s =>
    "scalafmt-cli-sbt/it:test" ::
      s
  },
  Command.command("ci-publish") { s =>
    s"very publish" :: s
  }
)
