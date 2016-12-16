resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("io.get-coursier"    % "sbt-coursier"        % "1.0.0-M14")
addSbtPlugin("com.eed3si9n"       % "sbt-assembly"        % "0.14.3")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"       % "1.0.1")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"             % "0.2.15")
addSbtPlugin("com.lihaoyi"        % "scalatex-sbt-plugin" % "0.3.5")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"        % "1.1")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"             % "1.0.0")
addSbtPlugin("org.xerial.sbt"     % "sbt-pack"            % "0.8.0")

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value
