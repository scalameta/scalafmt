resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayIvyRepo("jetbrains", "sbt-plugins")
)

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "1.2.7")
addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.6.0-RC4")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.2.2")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
addSbtPlugin(
  "io.get-coursier" % "sbt-coursier" % coursier.util.Properties.version
)
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.7")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.5.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.25")
// Don't upgrade sbt-idea-plugin until this is resolved:
// https://github.com/JetBrains/sbt-idea-plugin/commit/d56a8c0641329eddeb340baa0a15a3786d0c73f8#commitcomment-29963323
addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "1.0.1")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")
