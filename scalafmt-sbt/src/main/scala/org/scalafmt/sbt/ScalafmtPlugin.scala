package org.scalafmt.sbt
import sbt._, Keys._

object ScalafmtPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements
  object autoImport {
    val scalafmt: Command =
      Command.args("scalafmt", "run the scalafmt command line interface.") {
        case (state, args) =>
          org.scalafmt.cli.Cli.main("--non-interactive" +: args.toArray)
          state
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
