package org.scalafmt.cli

import scala.collection.mutable
import java.io.File

import metaconfig.Result
import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps


object StyleCache {
  private val styleCache = mutable.Map.empty[String, ScalafmtConfig]

  private val timeStamps = mutable.Map.empty[String, Long]

  def getStyleForFile(filename: String): Option[ScalafmtConfig] = {
    getStyleForFileOrError(filename).right.toOption
  }

  def getStyleForFileOrError(filename: String): Result[ScalafmtConfig] = {
    val file = new File(filename)
    val lastModified = file.lastModified()
    val configChanged = timeStamps.get(filename).contains(lastModified)
    timeStamps.update(filename, lastModified)

    styleCache.get(filename) match {
      case Some(config) if configChanged =>
        Right[Throwable,ScalafmtConfig](config)
      case None => {
        // Throw an exception if file does not exist. Better to fail fast than
        // continue silently.
        config.Config
          .fromHocon(FileOps.readFile(file))
          .right.map { style =>
          // Cache result forever. I prefer to create a nice IDE-agnostic UI for
          // experimenting with different config flags.
          styleCache.put(filename, style)
          style
        }
      }
    }
  }
}
