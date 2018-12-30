package org.scalafmt.cli

import java.nio.file.Path
import org.scalafmt.interfaces

final case class ScalafmtBuilderImpl(impl: ScalafmtImpl)
    extends interfaces.ScalafmtBuilder {
  def this() = this(ScalafmtImpl())
  override def withRespectExcludeFilters(
      respectExcludeFilters: Boolean
  ): interfaces.ScalafmtBuilder = {
    copy(impl.copy(respectExcludeFilters = respectExcludeFilters))
  }
  override def withConfig(config: Path): interfaces.ScalafmtBuilder = {
    copy(impl.copy(configPath = Some(config)))
  }
  override def withReporter(
      reporter: interfaces.ScalafmtReporter
  ): interfaces.ScalafmtBuilder = {
    copy(impl.copy(reporter = reporter))
  }
  override def create(): interfaces.Scalafmt = impl
}
