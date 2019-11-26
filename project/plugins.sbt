resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayIvyRepo("jetbrains", "sbt-plugins")
)

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "1.3.6")
addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.6.0-RC4")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.4.31")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
addSbtPlugin(
  "io.get-coursier" % "sbt-coursier" % coursier.util.Properties.version
)
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.7")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.5.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.25")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.5.1")
