package org.scalafmt.cli

import scala.collection.mutable

import java.io.File

import org.scalafmt.ScalafmtStyle
import org.scalafmt.cli.Cli.Config
import org.scalafmt.util.FileOps

object StyleCache {
  private val styleCache = mutable.Map.empty[String, Config]

  private val timeStamps = mutable.Map.empty[String, Long]
  def getConfigForFile(filename: String): Option[Config] = {
    val file = new File(filename)
    val lastModified = file.lastModified()
    val configChanged = timeStamps.get(filename).contains(lastModified)
    timeStamps.update(filename, lastModified)
    if (styleCache.contains(filename) && configChanged)
      styleCache.get(filename)
    else {
      // Throw an exception if file does not exist. Better to fail fast than
      // continue silently.
      Cli.parseConfigFile(FileOps.readFile(file)).map { config =>
        // Cache result forever. I prefer to create a nice IDE-agnostic UI for
        // experimenting with different config flags.
        styleCache.put(filename, config)
        config
      }
    }

  }
  def getStyleForFile(filename: String): Option[ScalafmtStyle] = {
    getConfigForFile(filename).map(_.style)
  }
}
