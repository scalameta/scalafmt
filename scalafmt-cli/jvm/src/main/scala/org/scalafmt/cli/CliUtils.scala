package org.scalafmt.cli

import org.scalafmt.sysops.AbsoluteFile

import com.martiansoftware.nailgun.NGContext

private[scalafmt] trait CliUtils {
  protected val isScalaNative: Boolean = false

  def nailMain(nGContext: NGContext): Unit = {
    val workingDirectory = AbsoluteFile
      .fromPathIfAbsolute(nGContext.getWorkingDirectory).getOrElse(
        throw new IllegalStateException(
          s"Expected absolute path, " +
            s"obtained nGContext.getWorkingDirectory = ${nGContext
                .getWorkingDirectory}",
        ),
      )
    val exit = Cli.mainWithOptions(
      nGContext.getArgs,
      CliOptions.default.copy(common =
        CliOptions.default.common.copy(
          cwd = Some(workingDirectory),
          out = nGContext.out,
          in = nGContext.in,
          err = nGContext.err,
        ),
      ),
    )
    nGContext.exit(exit.code)
  }

  protected def getDynamicRunner(): ScalafmtRunner = ScalafmtDynamicRunner

}
