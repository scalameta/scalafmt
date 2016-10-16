lazy val issue485 =
  project.in(file(".")).enablePlugins(AutomateHeaderPlugin, GitVersioning)

libraryDependencies ++= Vector(
  Library.scalaTest % "test"
)

initialCommands := """|import de.heikoseeberger.issue485._
                      |""".stripMargin
