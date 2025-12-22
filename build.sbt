import scala.scalanative.build._
import scala.util.Properties

import org.scalajs.linker.interface.ESVersion

import Dependencies._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

def isCI = System.getenv("CI") != null

def scala212 = "2.12.21"
def scala213 = "2.13.18"

def isScalaVer(ver: String) = Def.setting(scalaBinaryVersion.value == ver)
def isScala212 = isScalaVer("2.12")
def isScala213 = isScalaVer("2.13")

inThisBuild {
  List(
    // version is set dynamically by sbt-dynver, but let's adjust it
    version := {
      val curVersion = version.value
      def dynVer(out: sbtdynver.GitDescribeOutput): String =
        if (out.isCleanAfterTag || isCI) curVersion
        else s"${out.ref.dropPrefix}-next-SNAPSHOT" // modified for local builds
      dynverGitDescribeOutput.value.mkVersion(dynVer, curVersion)
    },
    organization := "org.scalameta",
    homepage := Some(url("https://github.com/scalameta/scalafmt")),
    licenses :=
      List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(Developer(
      "olafurpg",
      "Ólafur Páll Geirsson",
      "olafurpg@gmail.com",
      url("https://geirsson.com"),
    )),
    scalaVersion := scala213,
    crossScalaVersions := List(scala213, scala212),
    resolvers += Resolver.sonatypeCentralSnapshots,
    testFrameworks += new TestFramework("munit.Framework"),
    // causes native image issues
    dependencyOverrides += "org.jline" % "jline" % "3.30.6",
  )
}

name := "scalafmtRoot"
publish / skip := true

lazy val runAssembly = inputKey[Unit]("Run assembly")

lazy val copyScalaNative = taskKey[Unit]("Copy Scala Native output to root")

copyScalaNative := {
  val binaryVersion = (cli.native / scalaBinaryVersion).value
  val suffix = if (Properties.isWin) ".exe" else ""
  val nativeOutput = (cli.native / Compile / target).value /
    s"scala-$binaryVersion" / s"scalafmt-cli$suffix"
  val output = baseDirectory.value / s"scalafmt$suffix"
  IO.copyFile(nativeOutput, output)
}

addCommandAlias("native-image", "cli/nativeImage")
addCommandAlias(
  "scala-native",
  "cliNative/compile;cliNative/nativeLink;copyScalaNative",
)
addCommandAlias("test-jvm", "tests/test;cli/test")
addCommandAlias("test-js", "testsJS/test;cliJS/test")
addCommandAlias("test-native", "testsNative/test;cliNative/test")

lazy val dynamic = crossProject(JVMPlatform) // don't build for NativePlatform
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-dynamic")).settings(
    moduleName := "scalafmt-dynamic",
    description := "Implementation of scalafmt-interfaces",
    buildInfoSettings("org.scalafmt.dynamic", "BuildInfo"),
    libraryDependencies ++= List(
      "io.get-coursier" %% "coursier" % coursier,
      "com.typesafe" % "config" % "1.4.5",
    ),
    sharedTestSettings,
    scalacOptions ++= scalacJvmOptions.value,
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "versions", "9", "module-info.class") =>
        MergeStrategy.discard
      case PathList("META-INF", "sisu", "javax.inject.Named") =>
        MergeStrategy.concat
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },
  ).dependsOn(interfaces, sysops).dependsOn(core % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val interfaces = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-interfaces")).settings(
    moduleName := "scalafmt-interfaces",
    description :=
      "Dependency-free, pure Java public interfaces to integrate with Scalafmt through a build tool or editor plugin.",
    Compile / resourceGenerators += Def.task {
      val out = (Compile / managedResourceDirectories).value.head /
        "scalafmt.properties"
      val props = new java.util.Properties()
      props.put("version", version.value)
      IO.write(props, "scalafmt properties", out)
      List(out)
    },
  ).jvmSettings(
    javacOptions ++= Seq("-source", "8", "-target", "8"),
    Compile / doc / javacOptions := Seq("-Xdoclint:none", "-quiet"),
    Compile / doc / scalacOptions ++=
      Seq("-no-link-warnings", "-Wconf:cat=doc:silent"),
    crossVersion := CrossVersion.disabled,
    autoScalaLibrary := false,
  )

lazy val sysops = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-sysops")).settings(
    moduleName := "scalafmt-sysops",
    description := "Scalafmt systems operations",
    scalacOptions ++= scalacJvmOptions.value,
    sharedTestSettings,
  ).jsEnablePlugins(ScalaJSPlugin).jsSettings(
    libraryDependencies += "org.scalameta" %%% "io" % scalametaV,
    scalaJsSettings,
  )

lazy val config = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-config")).settings(
    moduleName := "scalafmt-config",
    description := "Scalafmt config parsing",
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies += metaconfigCore.value,
  ).jvmSettings(libraryDependencies += metaconfigTypesafe.value)
  .platformsSettings(NativePlatform, JSPlatform)(
    libraryDependencies += metaconfigSconfig.value,
  ).jsSettings(scalaJsSettings)

lazy val core = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .in(file("scalafmt-core")).settings(
    moduleName := "scalafmt-core",
    buildInfoSettings("org.scalafmt", "Versions"),
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= Seq("org.scalameta" %%% "mdoc-parser" % mdocV),
    libraryDependencies ++= {
      if (!isScala212.value) Nil
      else Seq(compilerPlugin(
        "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full,
      ))
    },
  )
  .nativeSettings(libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.1.1")
  .aggregate(sysops, config, macros).dependsOn(sysops, config, macros)
  .enablePlugins(BuildInfoPlugin)
lazy val coreJVM = core.jvm

lazy val macros = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .in(file("scalafmt-macros")).settings(
    moduleName := "scalafmt-macros",
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies += scalameta.value,
    libraryDependencies +=
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  )

import sbtassembly.AssemblyPlugin.defaultUniversalScript

val scalacJvmOptions = Def.setting {
  val cross =
    if (!isScala213.value) Nil
    else Seq("-Ymacro-annotations", "-Xfatal-warnings", "-deprecation:false")

  val unused = Seq("imports", "privates", "locals", "patvars", "implicits")
    .map(x => s"-Ywarn-unused:$x")

  cross ++ unused ++ Seq("-target:8", "-release:8")
}

lazy val cli = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-cli")).settings(
    moduleName := "scalafmt-cli",
    assembly / aggregate := false,
    assembly / mainClass := Some("org.scalafmt.cli.Cli"),
    assembly / assemblyOption := (assembly / assemblyOption).value
      .withPrependShellScript(Some(defaultUniversalScript(shebang = false))),
    assembly / assemblyJarName := "scalafmt.jar",
    assembly / assemblyMergeStrategy := {
      case "reflect.properties" => MergeStrategy.first
      case PathList("scala-collection-compat.properties") => MergeStrategy.first
      case PathList("META-INF", "versions", "9", "module-info.class") =>
        MergeStrategy.discard
      case PathList("META-INF", "sisu", "javax.inject.Named") =>
        MergeStrategy.concat
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit-diff" % munitV,
      "com.github.scopt" %%% "scopt" % "4.1.0",
    ),
    scalacOptions ++= scalacJvmOptions.value,
    Compile / mainClass := Some("org.scalafmt.cli.Cli"),
    sharedTestSettings,
  ).jvmSettings(
    libraryDependencies += "com.facebook" % "nailgun-server" % "1.0.1",
    nativeImageInstalled := isCI,
    nativeImageOptions += "-march=compatibility",
    nativeImageOptions ++= {
      // https://www.graalvm.org/22.3/reference-manual/native-image/guides/build-static-executables/
      // https://www.graalvm.org/latest/reference-manual/native-image/guides/build-static-executables/
      sys.env.get("NATIVE_IMAGE_STATIC") match {
        case Some("nolibc") => Seq(
            "-H:+UnlockExperimentalVMOptions",
            "-H:+StaticExecutableWithDynamicLibC",
            "-H:-UnlockExperimentalVMOptions",
          )
        case Some("musl") => Seq("--static", "--libc=musl")
        case _ => Nil
      }
    },
    runAssembly := {
      val jar = (assembly / assemblyOutputPath).value
      val args = sbt.complete.DefaultParsers.spaceDelimited("<args>").parsed
      val cmd = Seq("java", "-jar", jar.getAbsolutePath) ++ args
      val exit = scala.sys.process.Process(cmd).!
      if (exit != 0) sys.error(s"runAssembly failed with exit code $exit")
    },
  ).nativeSettings(scalaNativeConfig).dependsOn(core, interfaces)
  // TODO: enable NPM publishing
  .jsSettings(scalaJsSettings, scalaJSUseMainModuleInitializer := true)
  .jvmEnablePlugins(NativeImagePlugin)
  .jvmConfigure(_.dependsOn(dynamic.jvm).aggregate(dynamic.jvm))

lazy val tests = crossProject(JVMPlatform, NativePlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-tests")).settings(
    publish / skip := true,
    sharedTestSettings,
    libraryDependencies += scalametaTestkit.value % Test,
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1" % Test,
    scalacOptions ++= scalacJvmOptions.value,
    buildInfoPackage := "org.scalafmt.tests",
    buildInfoKeys := Seq[BuildInfoKey]("resourceDirectory" -> {
      val sharedTests = (baseDirectory.value.getParentFile / "shared").toPath
      (Test / resourceDirectories).value.find(_.toPath.startsWith(sharedTests))
        .get
    }),
  ).enablePlugins(BuildInfoPlugin).dependsOn(core).aggregate(core)
  .jvmSettings(javaOptions += "-Dfile.encoding=UTF8", parallelCollections)
  .jsSettings(scalaJsSettings).jsEnablePlugins(ScalaJSPlugin)

lazy val sharedTestSettings = Seq(libraryDependencies += munit.value % Test)

lazy val communityTestsCommon = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .configureCross(confCommunityTestShared("scalafmt-tests-community/common"))
  .dependsOn(core)

lazy val communityTestsScala2 = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .configureCross(confCommunityTest("scalafmt-tests-community/scala2"))

lazy val communityTestsScala3 = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .configureCross(confCommunityTest("scalafmt-tests-community/scala3"))

lazy val communityTestsSpark = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .configureCross(confCommunityTest("scalafmt-tests-community/spark"))

lazy val communityTestsIntellij = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .configureCross(confCommunityTest("scalafmt-tests-community/intellij"))

lazy val communityTestsOther = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .configureCross(confCommunityTest("scalafmt-tests-community/other"))

def confCommunityTestShared(where: String)(
    project: sbtcrossproject.CrossProject,
) = project.in(file(where)).settings(communityTestsSettings)
  .settings(sharedTestSettings).nativeSettings(scalaNativeConfig)

def confCommunityTest(where: String)(project: sbtcrossproject.CrossProject) =
  project.configureCross(confCommunityTestShared(where))
    .dependsOn(communityTestsCommon % "test->test")

lazy val benchmarks = project.in(file("scalafmt-benchmarks")).settings(
  publish / skip := true,
  moduleName := "scalafmt-benchmarks",
  libraryDependencies += scalametaTestkit.value,
  run / javaOptions ++= Seq(
    "-Djava.net.preferIPv4Stack=true",
    "-XX:+AggressiveOpts",
    "-XX:+UseParNewGC",
    "-XX:+UseConcMarkSweepGC",
    "-XX:+CMSParallelRemarkEnabled",
    "-XX:+CMSClassUnloadingEnabled",
    "-XX:ReservedCodeCacheSize=128m",
    "-XX:MaxMetaspaceSize=1024m",
    "-XX:SurvivorRatio=128",
    "-XX:MaxTenuringThreshold=0",
    "-Xss8M",
    "-Xms512M",
    "-Xmx2G",
    "-server",
  ),
).dependsOn(coreJVM).enablePlugins(JmhPlugin)

lazy val docs = project.in(file("scalafmt-docs")).settings(
  crossScalaVersions := List(scala212),
  publish / skip := true,
  mdoc := (Compile / run).evaluated,
).dependsOn(cli.jvm, dynamic.jvm).enablePlugins(DocusaurusPlugin)

val V = "\\d+\\.\\d+\\.\\d+"
val ReleaseCandidate = s"($V-RC\\d+).*".r
val Milestone = s"($V-M\\d+).*".r

lazy val stableVersion = Def
  .setting((ThisBuild / version).value.replaceAll("\\+.*", ""))

def buildInfoSettings(pkg: String, obj: String): Seq[Def.Setting[_]] = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    "scalameta" -> scalametaV,
    "nightly" -> version.value,
    "stable" -> stableVersion.value,
    "previousStable" ->
      previousStableVersion.value.getOrElse(stableVersion.value),
    "scala" -> scalaVersion.value,
    "scala212" -> scala212,
    "coursier" -> coursier,
    "commit" -> sys.process.Process("git rev-parse HEAD").lineStream_!.head,
    "timestamp" -> System.currentTimeMillis().toString,
    scalaVersion,
    sbtVersion,
  ),
  buildInfoPackage := pkg,
  buildInfoObject := obj,
)

lazy val communityTestsSettings: Seq[Def.Setting[_]] = Seq(
  publish / skip := true,
  scalacOptions ++= scalacJvmOptions.value,
  javaOptions += "-Dfile.encoding=UTF8",
)

lazy val scalaJsSettings = Seq(
  // to support Node.JS functionality
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  // to support MULTILINE in regex
  scalaJSLinkerConfig ~= (_.withESFeatures(_.withESVersion(ESVersion.ES2018))),
)

lazy val scalaNativeConfig = nativeConfig ~= { _.withMode(Mode.releaseFull) }

def parallelCollections = libraryDependencies ++= {
  if (!isScala213.value) Nil
  else Seq("org.scala-lang.modules" %%% "scala-parallel-collections" % "1.2.0")
}
