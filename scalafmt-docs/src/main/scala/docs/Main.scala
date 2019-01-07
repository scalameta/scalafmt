package docs

import java.nio.file.Paths
import org.scalafmt.Versions

object Main {

  def main(args: Array[String]): Unit = {
//    val docusaurus = Docusaurus.start()
    val settings = mdoc
      .MainSettings()
      .withOut(Paths.get("website", "target", "docs"))
      .withSiteVariables(
        Map(
          "VERSION" -> Versions.version,
          "STABLE_VERSION" -> Versions.stable,
          "SCALAMETA_VERSION" -> Versions.scalameta
        )
      )
      .withStringModifiers(
        List(
          new FileModifier,
          new ScalafmtModifier,
          new DefaultsModifier
        )
      )
      .withArgs(args.toList)
    val exit = mdoc.Main.process(settings)

    if (exit != 0) sys.exit(exit)
  }
}
