package org.scalafmt.dynamic

import java.nio.file.Path

import scala.util.Try

import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.dynamic.exceptions._
import org.scalafmt.interfaces._

final case class ScalafmtDynamicSession(
    properties: ScalafmtProperties,
    cfg: ScalafmtReflectConfig
) extends ScalafmtSession {

  import ScalafmtDynamicSession._

  override def format(file: Path, code: String): String =
    tryFormat(file, code).getOrElse(code)

  override def matchesProjectFilters(file: Path): Boolean =
    cfg.isIncludedInProject(file)

  override def isGitOnly: Boolean = cfg.projectIsGit

  private def tryFormat(
      file: Path,
      code: String
  ): FormatResult =
    if (properties.respectExcludeFilters && !matchesProjectFilters(file)) {
      properties.reporter.excluded(file)
      Right(code)
    } else
      tryForceFormat(file, code)

  private def tryForceFormat(
      file: Path,
      code: String
  ): FormatResult = {
    val needSbt = cfg.getVersion < ScalafmtVersion(3, 0, 0, 7) && {
      val extension = getExtension(file.toString)
      extension == "md" || // added in 3.0.0-RC7
      cfg.getVersion < ScalafmtVersion(2, 6, 3) &&
      extension == "sbt" || extension == "sc" // added in 2.6.3
    }
    Try {
      val configWithDialect: ScalafmtReflectConfig =
        if (needSbt) cfg.withSbtDialect else cfg
      configWithDialect.format(code, Some(file))
    }.toEither.left.map { x =>
      val cause = ReflectionException.flatten(x)
      properties.reporter.error(file, cause)
      UnknownError(cause)
    }
  }

}

private object ScalafmtDynamicSession {

  def getExtension(path: String): String = {
    val dotIndex = path.lastIndexOf('.')
    if (dotIndex < 0) "" else path.substring(dotIndex + 1)
  }

}
