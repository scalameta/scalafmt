resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.bintrayIvyRepo("dancingrobot84", "sbt-plugins")
)

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.1")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "1.2.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC12")
addSbtPlugin("com.eed3si9n" % "sbt-doge" % "0.1.5")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.27")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.20")
addSbtPlugin("com.lihaoyi" % "scalatex-sbt-plugin" % "0.3.9")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")
addSbtPlugin("com.dancingrobot84" % "sbt-idea-plugin" % "0.4.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value
