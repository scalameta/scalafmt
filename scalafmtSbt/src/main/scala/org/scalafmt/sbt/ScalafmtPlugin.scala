package org.scalafmt.sbt

import org.scalafmt.Versions
import org.scalafmt.bootstrap.ScalafmtBootstrap
import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin

object ScalafmtPlugin extends AutoPlugin {
  object autoImport {
    lazy val scalafmt: Command =
      Command.args("scalafmt", "run the scalafmt command line interface.") {
        case (s, args) =>
          try {
            ScalafmtBootstrap.main("--non-interactive" +: args)
          } catch {
            case e: java.lang.NoSuchMethodError
                if e.getMessage.startsWith("coursier") =>
              System.err.println(
                "Error. Found conflicting version of coursier, sbt-scalafmt requires" +
                  s" coursier version ${Versions.coursier}.")
          }
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

  override def trigger: PluginTrigger = allRequirements
  override def requires = JvmPlugin
}
