package docs

import org.scalafmt.Versions

import java.nio.file.Paths

object Main {

  def main(args: Array[String]): Unit = {
    val argsList = args.toList
    // Opt in to failing on dead links with -Dmdoc.linkHygiene=true. Since
    // docusaurusCreateSite invokes mdoc with no args, a system property is how
    // CI turns the check on without a separate mdoc run (a CLI arg wouldn't
    // reach that invocation). An explicit link-hygiene arg takes precedence.
    val needLinkHygiene = sys.props.get("mdoc.linkHygiene").contains("true") &&
      !args.exists(a => a == "--check-link-hygiene" || a == "--no-link-hygiene")
    val withLinkHygiene =
      if (needLinkHygiene) "--check-link-hygiene" :: argsList else argsList
    val settings = mdoc.MainSettings().withArgs(withLinkHygiene)
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
