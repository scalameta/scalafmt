resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayIvyRepo("jetbrains", "sbt-plugins")
)

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.5")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.3")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin(
  "io.get-coursier" % "sbt-coursier" % coursier.util.Properties.version
)
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.1.1")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.5")
