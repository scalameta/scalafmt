package org.scalafmt.sbt

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object ScalafmtPlugin extends AutoPlugin {
  private def baseCmd = "scalafmt-stub/runMain org.scalafmt.cli.Cli "
  private def cmd(args: String*)(s: State): State =
    s"$baseCmd ${args.mkString(" ")}" :: s
  object autoImport {

    lazy val scalafmt: Command =
      Command.args("scalafmt", "run the scalafmt command line interface.") {
        case (s, args) => s"$baseCmd ${args.mkString(" ")}" :: s
      }
    // These are not strictly necessary, since they can be run with the
    // scalafmt command. However, they're convenient to avoid the need for
    // parentheses when running `sbt "scalafmt --test"`.
    lazy val scalafmtTest: Command =
      Command.command("scalafmtTest")(cmd("--test"))
    lazy val scalafmtDiff: Command =
      Command.command("scalafmtDiff")(cmd("--diff"))
    lazy val scalafmtDiffTest: Command =
      Command.command("scalafmtDiffTest")(cmd("--diff", "--test"))
  }
  import autoImport._
  lazy val scalafmtStub: Project = Project(
    id = "scalafmt-stub",
    base = file("project/scalafmt")
  ).settings(
    scalaVersion := "2.11.8",
    libraryDependencies +=
      "com.geirsson" %% "scalafmt-cli" % _root_.org.scalafmt.Versions.nightly
  )
  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    commands ++= {
      if (sbtVersion.value < "0.13.13") {
        // can't do streams.value.log.warn since streams is a task and
        // settings like `commands` can't depend on tasks.
        val warn = s"[${scala.Console.YELLOW}warn${scala.Console.RESET}]"
        System.err.println(
          s"$warn sbt-scalafmt requires sbt.version=0.13.13 or higher. " +
            s"Please upgrade sbt to expose the `scalafmt` command.")
        Nil
      } else {
        Seq(scalafmt, scalafmtTest, scalafmtDiff, scalafmtDiffTest)
      }
    }
  )
  override def extraProjects: Seq[Project] = Seq(scalafmtStub)
  override def trigger: PluginTrigger = allRequirements
  override def requires = JvmPlugin
}
