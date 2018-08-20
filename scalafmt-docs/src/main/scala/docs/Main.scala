package docs

import java.nio.file.Paths
import org.scalafmt.Versions

object Main {

  def main(args: Array[String]): Unit = {
//    val docusaurus = Docusaurus.start()
    val settings = vork
      .MainSettings()
      .withCleanTarget(true)
      .withWatch(args.contains("-w"))
      .withOut(Paths.get("website", "target", "docs"))
      .withSiteVariables(
        Map(
          "VERSION" -> Versions.version,
          "STABLE_VERSION" -> Versions.stable,
          "SCALAMETA_VERSION" -> Versions.scalameta
        )
      )
    val exit = vork.Main.process(settings)
//    docusaurus.kill()
    sys.exit(exit)
  }
}
