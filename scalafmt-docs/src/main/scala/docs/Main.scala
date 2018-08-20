package docs

import java.nio.file.Paths
import org.scalafmt.Versions

object Main {

  def main(args: Array[String]): Unit = {
//    val docusaurus = Docusaurus.start()
    val settings = vork
      .MainSettings()
//      .withCleanTarget(true)
      .withWatch(args.contains("-w"))
      .withOut(Paths.get("website", "target", "docs"))
      .withIncludePath(
        List(
          // Uncomment if you want to generate only a single file,
          // useful for getting rapid feedback while iterating on a
          // custom modifier.
          // FIXME: https://github.com/olafurpg/vork/issues/50
          // FileSystems.getDefault.getPathMatcher("glob:configuration.md")
        )
      )
      .withSiteVariables(
        Map(
          "VERSION" -> Versions.version,
          "STABLE_VERSION" -> Versions.stable,
          "SCALAMETA_VERSION" -> Versions.scalameta
        )
      )
      .withStringModifiers(
        List(
          new ScalafmtModifier,
          new DefaultsModifier
        )
      )
    val exit = vork.Main.process(settings)
//    docusaurus.kill()
    sys.exit(exit)
  }
}
