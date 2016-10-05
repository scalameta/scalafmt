package org.scalafmt.cli

import scala.collection.mutable

import java.io.File

import metaconfig.Result
import org.scalafmt.config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps
import org.scalafmt.util.logger

object StyleCache {
  private val styleCache = mutable.Map.empty[String, ScalafmtConfig]

  private val timeStamps = mutable.Map.empty[String, Long]

  def getStyleForFile(filename: String): Option[ScalafmtConfig] = {
    getStyleForFileOrError(filename).right.toOption
  }

  def getStyleForFileOrError(filename: String): Result[ScalafmtConfig] = {
    val file = new File(filename)
    val lastModified = file.lastModified()
    val configUnchanged = timeStamps.get(filename).contains(lastModified)
    timeStamps.update(filename, lastModified)
    styleCache.get(filename) match {
      case Some(config) if configUnchanged => Right(config)
      case _ =>
        // Throw an exception if file does not exist. Better to fail fast than
        // continue silently.
        val result = config.Config.fromHocon(FileOps.readFile(file))
        result match {
          case Left(e) =>
            styleCache.remove(filename)
          case Right(style) =>
            styleCache.put(filename, style)
        }
        result
    }
  }
}
