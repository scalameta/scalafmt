// scalafmt: { maxColumn = 120 }

import sbt.*
import sbt.Keys.*

import scala.scalanative.build.Mode
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.*

import org.scalajs.linker.interface.{ESVersion, ModuleKind}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

object Extensions {

  import Dependencies.*

  // Leaves the JVM/2.13 cell unsuffixed; left alone a matrix unsuffixes JVM/Scala 3.
  def bareAxes: Seq[VirtualAxis] = Seq(VirtualAxis.jvm, VirtualAxis.scalaABIVersion(scala213))

  // sbt runs a `;`-separated list; the leading separator is required
  def tasks(ts: Seq[String]): String = ts.mkString("; ", "; ", "")

  def tasksOf(p: Project, ts: String*): Seq[String] = ts.map(t => s"${p.id}/$t")

  // `++<version>` selects no row, so every version gets its own alias. Cell ids are generated, so
  // the names are taken from them rather than spelled out.
  def testAliases(versions: Seq[String], matrices: ProjectMatrix*): Seq[Setting[?]] = versions.flatMap { v =>
    def alias(name: String, f: ProjectMatrix => ProjectFinder) = addCommandAlias(
      s"test-$name-${VirtualAxis.scalaABIVersion(v).idSuffix}",
      tasks(matrices.map(m => s"${f(m)(v).id}/testFull")),
    )
    alias("jvm", _.jvm) ++ alias("js", _.js) ++ alias("native", _.native)
  }

  // A matrix has one base directory, so a cell has to name every tree it reads. Directories that
  // do not exist are harmless.
  private def roots(base: File, dirs: String*): Seq[Setting[?]] = {
    def under(conf: String, leaf: String => Seq[String]) = Def.setting {
      // a matrix base may be relative, and a relative source root resolves against the wrong one
      val root = IO.resolve((ThisBuild / baseDirectory).value, base)
      for (dir <- dirs.toList; name <- leaf(scalaBinaryVersion.value)) yield root / dir / "src" / conf / name
    }
    def sources(sbv: String) = Seq("scala", "java", s"scala-$sbv", s"scala-${sbv.head}").distinct
    Def.settings(
      Compile / unmanagedSourceDirectories ++= under("main", sources).value,
      Test / unmanagedSourceDirectories ++= under("test", sources).value,
      Compile / unmanagedResourceDirectories ++= under("main", _ => Seq("resources")).value,
      Test / unmanagedResourceDirectories ++= under("test", _ => Seq("resources")).value,
    )
  }

  def isScalaVer(ver: String) = Def.setting(scalaBinaryVersion.value == ver)
  def isScala212 = isScalaVer("2.12")
  def isScala213 = isScalaVer("2.13")
  def isScala3 = isScalaVer("3")

  val unpublished = publish / skip := true

  lazy val sharedTestSettings = Seq(libraryDependencies += munit.value % Test)

  val scalacJvmOptions = Def.setting {
    val cross = if (!isScala213.value) Nil else Seq("-Ymacro-annotations")

    val warningAsError =
      if (isScala212.value) Seq("-Xfatal-warnings", "-deprecation:false")
      else Seq("-Wconf:any:error,cat=deprecation:silent")

    val unused =
      if (isScala3.value) "-Wunused:all"
      else if (isScala213.value) "-Wunused:imports,privates,locals,patvars,implicits,explicits,params"
      else "-Ywarn-unused:imports,privates,locals,patvars,implicits"

    val javaver = if (isScala3.value) Seq("-java-output-version:8") else Seq("-target:8", "-release:8")

    cross ++ warningAsError ++ javaver :+ unused
  }

  val scalacSettings = Def.settings(
    javacOptions ++= Seq("-source", "8", "-target", "8"),
    Compile / compile / scalacOptions ++= scalacJvmOptions.value,
    Test / compile / scalacOptions ++= scalacJvmOptions.value,
  )

  lazy val scalaJsSettings = Seq(
    // to support Node.JS functionality
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    // to support MULTILINE in regex
    scalaJSLinkerConfig ~= (_.withESFeatures(_.withESVersion(ESVersion.ES2018))),
  )

  lazy val scalaNativeConfig = nativeConfig ~= { _.withMode(Mode.releaseFull) }

  def parallelCollections = libraryDependencies ++=
    { if (!isScala213.value) Nil else Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0") }

  lazy val communityTestsSettings: Seq[Def.Setting[?]] = Def
    .settings(unpublished, scalacSettings, sharedTestSettings, javaOptions += "-Dfile.encoding=UTF8")

  // `projectMatrix` is a macro that reads the name of the val it is assigned to, so it cannot be
  // called here; it arrives as the receiver instead.
  implicit class ProjectMatrixExtensions(private val self: ProjectMatrix) extends AnyVal {

    def apply(name: String, axes: VirtualAxis*): ProjectMatrix = {
      val axesToUse = if (axes.isEmpty) bareAxes else axes
      // off `in`'s result, not `self`: until then the base is still the val name
      val named = self.in(file(name)).defaultAxes(axesToUse *)
      named.settings(roots(named.base, "shared"))
    }

    def crossJvm(ss: Def.SettingsDefinition*): ProjectMatrix = self
      .jvmPlatform(scalaVersions, jvmRoots ++ ss.flatMap(_.settings))

    def crossJs(ss: Def.SettingsDefinition*): ProjectMatrix = self
      .jsPlatform(scalaVersions, roots(self.base, "js", "js-jvm", "js-native") ++ ss.flatMap(_.settings))

    def crossNative(ss: Def.SettingsDefinition*): ProjectMatrix = self
      .nativePlatform(scalaVersions, roots(self.base, "native", "jvm-native", "js-native") ++ ss.flatMap(_.settings))

    // A JVM row carrying no Scala version, for Java-only sources. Not
    // `jvmPlatform(autoScalaLibrary = false)`: that one passes VirtualAxis.jvm to a customRow which
    // appends it again, and the doubled axis renames the generated directories to `scalajvm-jvm`.
    // The cell id is unaffected, so it would just compile nothing.
    def crossJvmJava(ss: Def.SettingsDefinition*): ProjectMatrix = self
      .customRow(autoScalaLibrary = false, axisValues = Nil, settings = jvmRoots ++ ss.flatMap(_.settings))

    // a row that needs the cell itself, not just its settings
    def crossJvmRow(version: String, configure: Project => Project): ProjectMatrix = self
      .jvmPlatform(Seq(version), Nil, configure(_).settings(jvmRoots))

    // a row that needs the cell itself, not just its settings
    def crossJvmRow(versions: String*)(configure: String => Project => Project): ProjectMatrix = versions
      .foldLeft(self)((acc, version) => acc.crossJvmRow(version, configure(version)))

    def crossAll: ProjectMatrix = self.crossJvm().crossJs().crossNative()

    def crossJsNative: ProjectMatrix = self.crossJs().crossNative()

    def crossJvmNative(nativeOnly: Def.SettingsDefinition*): ProjectMatrix = self.crossJvm().crossNative(nativeOnly *)

    def communityTest: ProjectMatrix = self.settings(communityTestsSettings).crossJvmNative(scalaNativeConfig)

    private def jvmRoots = roots(self.base, "jvm", "jvm-native", "js-jvm")
  }

}
