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

  /* `++<version>` selects no row, so every version gets its own alias. Cell ids are generated, so
   * the names are taken from them rather than spelled out. */
  def testAliases(versions: Seq[String], matrices: ProjectMatrix*): Seq[Setting[?]] = versions.flatMap { v =>
    def alias(name: String, f: ProjectMatrix => ProjectFinder) = addCommandAlias(
      s"test-$name-${VirtualAxis.scalaABIVersion(v).idSuffix}",
      tasks(matrices.map(m => s"${f(m)(v).id}/testFull")),
    )
    alias("jvm", _.jvm) ++ alias("js", _.js) ++ alias("native", _.native)
  }

  /* `bspEnabled := false` leaves a row out of the BSP workspace, so an IDE does not import it.
   * IntelliJ can't load multiple versions, though, so force 2.13 if `ide.scala` is absent. */
  private val ideScala = {
    val prop = sys.props.getOrElse("ide.scala", "").trim
    if (prop.nonEmpty) Some(prop)
    else if (sys.props.contains("idea.managed")) Some(scala213) // this looks like IntelliJ
    else None
  }

  // this build exposes every platform; one that does not names its own set
  private val defaultPlatforms = Set.empty[String]

  // an empty set is no filter, so every platform
  private val idePlatforms = sys.props.get("ide.platform")
    .fold(defaultPlatforms)(_.split(',').map(_.trim).filter(_.nonEmpty).toSet)

  // only ever disables a row, so it never overrides another setting
  private def ideSkip(platform: VirtualAxis.PlatformAxis, version: String): Seq[Setting[?]] = {
    val skip = idePlatforms.nonEmpty && !idePlatforms(platform.value) ||
      version.nonEmpty && ideScala.exists(s => s != version && s != CrossVersion.binaryScalaVersion(version))
    if (skip) Seq(bspEnabled := false) else Nil
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

    // Scala 3.8 accepts no output version below 17
    val javaver = if (isScala3.value) Seq("-java-output-version:17") else Seq("-target:8", "-release:8")

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

  // `projectMatrix` reads the name of the val it is assigned to, so it arrives as the receiver
  implicit class ProjectMatrixExtensions(private val self: ProjectMatrix) extends AnyVal {

    def apply(name: String, axes: VirtualAxis*): ProjectMatrix = {
      val axesToUse = if (axes.isEmpty) bareAxes else axes
      // off `in`'s result, not `self`: until then the base is still the val name
      self.in(file(name)).defaultAxes(axesToUse *)
    }

    // one row at a time, so each one knows the version it is built for
    def crossJvm(ss: Def.SettingsDefinition*): ProjectMatrix = scalaVersions
      .foldLeft(self)((acc, v) => acc.jvmPlatform(Seq(v), jvmRoots(v, ss)))

    def crossJs(ss: Def.SettingsDefinition*): ProjectMatrix = scalaVersions
      .foldLeft(self)((acc, v) => acc.jsPlatform(Seq(v), jsRoots(v, ss)))

    def crossNative(ss: Def.SettingsDefinition*): ProjectMatrix = scalaVersions
      .foldLeft(self)((acc, v) => acc.nativePlatform(Seq(v), nativeRoots(v, ss)))

    /* A JVM row for Java-only sources, carrying no Scala version. Not
     * `jvmPlatform(autoScalaLibrary = false)`: that one passes VirtualAxis.jvm to a customRow which
     * appends it again, and the doubled axis renames the generated directories to `scalajvm-jvm`,
     * leaving the cell to compile nothing. */
    def crossJvmJava(ss: Def.SettingsDefinition*): ProjectMatrix = self
      .customRow(autoScalaLibrary = false, axisValues = Nil, settings = jvmRoots("", ss))

    // a JVM row for a project that does not cross-build
    def crossJvmAt(version: String): ProjectMatrix = self.jvmPlatform(Seq(version), jvmRoots(version))

    // a row that needs the cell itself, not just its settings
    def crossJvmRow(version: String, configure: Project => Project): ProjectMatrix = self
      .jvmPlatform(Seq(version), Nil, configure(_).settings(jvmRoots(version)))

    // a row that needs the cell itself, not just its settings
    def crossJvmRow(versions: String*)(configure: String => Project => Project): ProjectMatrix = versions
      .foldLeft(self)((acc, version) => acc.crossJvmRow(version, configure(version)))

    def crossAll: ProjectMatrix = self.crossJvm().crossJs().crossNative()

    def crossJsNative: ProjectMatrix = self.crossJs().crossNative()

    def crossJvmNative(nativeOnly: Def.SettingsDefinition*): ProjectMatrix = self.crossJvm().crossNative(nativeOnly *)

    def communityTest: ProjectMatrix = self.settings(communityTestsSettings).crossJvmNative(scalaNativeConfig)

    private def platformRoots(platform: VirtualAxis.PlatformAxis, version: String, ss: Seq[Def.SettingsDefinition]) =
      ideSkip(platform, version) ++ ss.flatMap(_.settings)

    private def jvmRoots(version: String, ss: Seq[Def.SettingsDefinition] = Nil) =
      platformRoots(VirtualAxis.jvm, version, ss)
    private def jsRoots(version: String, ss: Seq[Def.SettingsDefinition]) = platformRoots(VirtualAxis.js, version, ss)
    private def nativeRoots(version: String, ss: Seq[Def.SettingsDefinition]) =
      platformRoots(VirtualAxis.native, version, ss)
  }

}
