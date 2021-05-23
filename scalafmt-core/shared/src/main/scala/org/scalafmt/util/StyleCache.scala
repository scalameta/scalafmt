package org.scalafmt.util

import scala.collection.mutable

import java.io.File

import metaconfig.Configured
import metaconfig.Configured.Ok
import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig

object StyleCache {
  private val styleCache = mutable.Map.empty[String, ScalafmtConfig]

  private val timeStamps = mutable.Map.empty[String, Long]

  def getStyleForFile(filename: String): Option[ScalafmtConfig] = {
    getStyleForFileOrError(filename)
  }

  def getStyleForFileOrError(filename: String): Configured[ScalafmtConfig] = {
    val file = new File(filename)
    val lastModified = file.lastModified()
    val configUnchanged = timeStamps.get(filename).contains(lastModified)
    timeStamps.update(filename, lastModified)
    styleCache.get(filename) match {
      case Some(config) if configUnchanged => Ok(config)
      case _ =>
        // Throw an exception if file does not exist. Better to fail fast than
        // continue silently.
        val result = config.Config.fromHoconFile(file, None)
        result match {
          case Configured.NotOk(e) =>
            styleCache.remove(filename)
          case Configured.Ok(style) =>
            styleCache.put(filename, style)
        }
        result
    }
  }
}
