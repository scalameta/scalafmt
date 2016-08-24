resolvers += Classpaths.sbtPluginReleases

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M13")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
addSbtPlugin("org.brianmckenna" % "sbt-wartremover" % "0.14")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.0.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.6")
addSbtPlugin("com.lihaoyi" % "scalatex-sbt-plugin" % "0.3.5")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")
