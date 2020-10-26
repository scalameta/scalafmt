resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayIvyRepo("jetbrains", "sbt-plugins")
)

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.9")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.3")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.2.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.8.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.5")
