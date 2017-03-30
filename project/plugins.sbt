resolvers ++= Seq(
  Classpaths.sbtPluginReleases,
  Resolver.bintrayIvyRepo("dancingrobot84", "sbt-plugins")
)

addSbtPlugin("me.lessis"          % "bintray-sbt"         % "0.3.0")
addSbtPlugin("com.dwijnand"       % "sbt-dynver"          % "1.2.0")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"       % "0.6.1")
addSbtPlugin("io.get-coursier"    % "sbt-coursier"        % "1.0.0-M15-5")
addSbtPlugin("com.eed3si9n"       % "sbt-doge"            % "0.1.5")
addSbtPlugin("com.eed3si9n"       % "sbt-assembly"        % "0.14.3")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"             % "0.2.15")
addSbtPlugin("com.lihaoyi"        % "scalatex-sbt-plugin" % "0.3.7")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"        % "1.1")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"             % "1.0.0")
addSbtPlugin("org.xerial.sbt"     % "sbt-pack"            % "0.8.0")
addSbtPlugin("com.dancingrobot84" % "sbt-idea-plugin"     % "0.4.2")

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value
