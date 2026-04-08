package docs

import org.scalafmt.Versions

import java.nio.file.Paths

object Main {

  def main(args: Array[String]): Unit = {
    val settings = mdoc.MainSettings().withArgs(args.toList)
      .withOut(Paths.get("website", "target", "docs")).withSiteVariables(Map(
        "VERSION" -> Versions.version,
        "STABLE_VERSION" -> Versions.stable,
        "SCALAMETA_VERSION" -> Versions.scalameta,
      )).withStringModifiers(List(
        new FileModifier,
        new ScalafmtModifier,
        new DefaultsModifier,
      ))
    val exit = mdoc.Main.process(settings)

    if (exit != 0) sys.exit(exit)
  }
}
