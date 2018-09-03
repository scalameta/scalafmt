import Dependencies._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

def scala211 = "2.11.12"
def scala212 = "2.12.6"

inThisBuild(
  List(
    organization := "com.geirsson", // not org.scalameta because that's a breaking change
    homepage := Some(url("https://github.com/scalameta/scalafmt")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "olafurpg",
        "Ólafur Páll Geirsson",
        "olafurpg@gmail.com",
        url("https://geirsson.com")
      )
    ),
    scalaVersion := scala212,
    crossScalaVersions := List(scala212, scala211),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= List(
      scalatest.value % Test,
      scalacheck % Test,
      scalametaTestkit % Test
    )
  )
)

name := "scalafmtRoot"
skip in publish := true
addCommandAlias("downloadIdea", "intellij/updateIdea")

commands += Command.command("ci-test") { s =>
  val scalaVersion = sys.env.get("TEST") match {
    case Some("2.11") => scala211
    case _ => scala212
  }
  s"++$scalaVersion" ::
    s"tests/test" ::
    s"coreJS/test" ::
    s
}

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .in(file("scalafmt-core"))
  .settings(
    moduleName := "scalafmt-core",
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
    ),
    buildInfoSettings,
    libraryDependencies ++= Seq(
      metaconfig.value,
      scalameta.value
    )
  )
  .jsSettings(
    libraryDependencies ++= List(
      metaconfigHocon.value,
      scalatest.value % Test // must be here for coreJS/test to run anything
    )
  )
  .jvmSettings(
    fork.in(run).in(Test) := true,
    libraryDependencies ++= List(
      metaconfigTypesafe.value
    )
  )
  .enablePlugins(BuildInfoPlugin)
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val cli = project
  .in(file("scalafmt-cli"))
  .settings(
    moduleName := "scalafmt-cli",
    mainClass in assembly := Some("org.scalafmt.cli.Cli"),
    assemblyJarName.in(assembly) := "scalafmt.jar",
    libraryDependencies ++= Seq(
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
      "com.martiansoftware" % "nailgun-server" % "0.9.1",
      "com.github.scopt" %% "scopt" % "3.5.0"
    )
  )
  .dependsOn(coreJVM)

lazy val big = project
  .in(file("scalafmt-big"))
  .settings(
    moduleName := "scalafmt-big",
    crossScalaVersions := List(scala212),
    mimaReportBinaryIssues := {},
    shadeSettings
  )
  .dependsOn(cli)

lazy val shadeSettings: List[Setting[_]] = List(
  assemblyOption.in(assembly) ~= { _.copy(includeScala = false) },
  assemblyShadeRules.in(assembly) := Seq(
    ShadeRule
      .rename(
        "scala.meta.**" -> "org.scalafmt.shaded.meta.@1",
        "fastparse.**" -> "org.scalafmt.shaded.fastparse.@1"
      )
      .inAll,
    ShadeRule
      .zap(
        "scalapb.**",
        "com.trueaccord.**"
      )
      .inAll
  ),
  artifact.in(Compile, packageBin) := artifact.in(Compile, assembly).value,
  pomPostProcess := { (node: scala.xml.Node) =>
    new scala.xml.transform.RuleTransformer(
      new scala.xml.transform.RewriteRule {
        override def transform(node: scala.xml.Node): scala.xml.NodeSeq =
          node match {
            case e: scala.xml.Elem
                if e.label == "dependency" &&
                  e.child.exists { child =>
                    child.label == "artifactId" &&
                    child.text.startsWith("scalafmt")
                  } =>
              scala.xml.Comment(s"shaded scalafmt-cli dependency.")
            case _ => node
          }
      }
    ).transform(node).head
  }
) ++ addArtifact(artifact.in(Compile, packageBin), assembly).settings

lazy val intellij = project
  .in(file("scalafmt-intellij"))
  .settings(
    buildInfoSettings,
    crossScalaVersions := List(scala211),
    skip in publish := true,
    sources in (Compile, doc) := Nil,
    mimaReportBinaryIssues := {},
    ideaBuild := "2016.3.2",
    test := {}, // no need to download IDEA to run all tests.
    ideaEdition := IdeaEdition.Community,
    ideaDownloadDirectory in ThisBuild := baseDirectory.value / "idea",
    libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
    cleanFiles += ideaDownloadDirectory.value
  )
  .dependsOn(coreJVM, cli)

lazy val tests = project
  .in(file("scalafmt-tests"))
  .settings(
    skip in publish := true,
    libraryDependencies ++= Seq(
      // Test dependencies
      "com.lihaoyi" %% "scalatags" % "0.6.3",
      "org.typelevel" %% "paiges-core" % "0.2.0",
      scalametaTestkit
    )
  )
  .dependsOn(
    cli
  )

lazy val benchmarks = project
  .in(file("scalafmt-benchmarks"))
  .settings(
    skip in publish := true,
    moduleName := "scalafmt-benchmarks",
    libraryDependencies ++= Seq(
      scalametaTestkit
    ),
    javaOptions in run ++= Seq(
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
      "-server"
    )
  )
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("scalafmt-docs"))
  .settings(
    crossScalaVersions := List(scala212),
    skip in publish := true,
    mainClass.in(Compile) := Some("docs.Main")
  )
  .dependsOn(cli)
  .enablePlugins(DocusaurusPlugin)

val V = "\\d+\\.\\d+\\.\\d+"
val ReleaseCandidate = s"($V-RC\\d+).*".r
val Milestone = s"($V-M\\d+).*".r

lazy val stableVersion = Def.setting {
  val latestStable = "1.5.1"
  version.value match {
    case ReleaseCandidate(_) => latestStable
    case Milestone(_) => latestStable
    case v => v.replaceAll("\\-.*", "")
  }
}

lazy val buildInfoSettings: Seq[Def.Setting[_]] = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    "scalameta" -> scalametaV,
    "nightly" -> version.value,
    "stable" -> stableVersion.value,
    "scala" -> scalaVersion.value,
    "coursier" -> coursier,
    "commit" -> sys.process.Process("git rev-parse HEAD").lineStream_!.head,
    "timestamp" -> System.currentTimeMillis().toString,
    scalaVersion,
    sbtVersion
  ),
  buildInfoPackage := "org.scalafmt",
  buildInfoObject := "Versions"
)
