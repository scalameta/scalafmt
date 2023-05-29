resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayIvyRepo("jetbrains", "sbt-plugins")
)

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.7")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.1")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.4")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.1")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.2")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")
