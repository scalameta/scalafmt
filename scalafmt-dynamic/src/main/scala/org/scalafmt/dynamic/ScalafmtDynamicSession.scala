package org.scalafmt.dynamic

import java.nio.file.Path

import scala.util.Success

import org.scalafmt.dynamic.exceptions._
import org.scalafmt.interfaces._

final case class ScalafmtDynamicSession(
    properties: ScalafmtProperties,
    cfg: ScalafmtReflectConfig
) extends ScalafmtSession {

  import ScalafmtDynamicSession._

  override def format(file: Path, code: String): String = tryFormat(file, code)
    .getOrElse(code)

  override def formatOrError(file: Path, code: String): ScalafmtResult =
    tryFormat(file, code).fold(new ScalafmtResult(_), new ScalafmtResult(_))

  override def matchesProjectFilters(file: Path): Boolean = cfg
    .isIncludedInProject(file)

  override def isGitOnly: Boolean = cfg.projectIsGit

  private def tryFormat(file: Path, code: String): FormatResult =
    if (properties.respectExcludeFilters && !matchesProjectFilters(file)) {
      properties.reporter.excluded(file)
      Right(code)
    } else tryForceFormat(file, code)

  private def tryForceFormat(file: Path, code: String): FormatResult = {
    val needSbt = cfg.getVersion < ScalafmtVersion(3, 0, 0, 7) && {
      val extension = getExtension(file.toString)
      extension == "md" || // added in 3.0.0-RC7
      cfg.getVersion < ScalafmtVersion(2, 6, 3) && extension == "sbt" ||
      extension == "sc" // added in 2.6.3
    }
    val cfgWithDialect = if (needSbt) cfg.withSbtDialect else Success(cfg)
    val formatResult = cfgWithDialect.flatMap(_.tryFormat(code, Some(file)))
    formatResult.toEither.left.map { x =>
      val cause = ReflectionException.flatten(x)
      properties.reporter.error(file, cause)
      new ScalafmtDynamicError("Format error", cause)
    }
  }

}

private object ScalafmtDynamicSession {

  def getExtension(path: String): String = {
    val dotIndex = path.lastIndexOf('.')
    if (dotIndex < 0) "" else path.substring(dotIndex + 1)
  }

}
