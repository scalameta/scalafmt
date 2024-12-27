// scalafmt: { maxColumn = 100, align.preset = more, align.allowOverflow = true }

resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.sonatypeRepo("releases"),
  Resolver.bintrayIvyRepo("jetbrains", "sbt-plugins"),
)

val crossProjectV = "1.3.2"

addSbtPlugin("com.eed3si9n" % "sbt-assembly"  % "2.3.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

addSbtPlugin("com.github.sbt" % "sbt-ci-release"      % "1.9.2")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.10.4")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % crossProjectV)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % crossProjectV)

addSbtPlugin("org.scalameta"    % "sbt-mdoc"         % "2.6.2")
addSbtPlugin("org.scalameta"    % "sbt-scalafmt"     % "2.5.2")
addSbtPlugin("org.scalameta"    % "sbt-native-image" % "0.3.4")
addSbtPlugin("org.scala-js"     % "sbt-scalajs"      % "1.17.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.6")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
