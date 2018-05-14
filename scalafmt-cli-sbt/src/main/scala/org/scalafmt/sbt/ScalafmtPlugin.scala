package org.scalafmt.sbt
import sbt._
import Keys._

object ScalafmtPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements
  object autoImport {
    val scalafmt: Command =
      Command.args("scalafmt", "run the scalafmt command line interface.") {
        case (s, args) =>
          org.scalafmt.cli.Cli.exceptionThrowingMain(
            "--non-interactive" +: args.toArray
          )
          s
      }
  }
  override def globalSettings: Seq[Def.Setting[_]] =
    Seq(
      commands += autoImport.scalafmt
    ) ++
      addCommandAlias("scalafmtTest", "scalafmt --test") ++
      addCommandAlias("scalafmtDiffTest", "scalafmt --diff --test") ++
      addCommandAlias("scalafmtDiff", "scalafmt --diff")

}
