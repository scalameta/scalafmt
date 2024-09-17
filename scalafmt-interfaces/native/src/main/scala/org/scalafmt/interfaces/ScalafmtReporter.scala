package org.scalafmt.interfaces

import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.file.Path

/** A reporter to handle error and information messages from Scalafmt.
  */
trait ScalafmtReporter {

  /** An error occurred while trying to process this file.
    *
    * @param file
    *   can be either a Scala source file or .scalafmt.conf.
    * @param message
    *   the error message.
    */
  def error(file: Path, message: String): Unit

  /** An exception occurred while trying to process this file.
    *
    * @param file
    *   can be either a Scala source file or .scalafmt.conf.
    * @param e
    *   the exception that occurred, has type {@link PositionException} when the
    *   error appeared as a position.
    */
  def error(file: Path, e: Throwable): Unit

  /** An exception occurred while trying to process this file.
    *
    * @param file
    *   can be either a Scala source file or .scalafmt.conf.
    * @param message
    *   additional error message
    * @param e
    *   the exception that occurred, has type {@link PositionException} when the
    *   error appeared as a position.
    */
  def error(file: Path, message: String, e: Throwable): Unit =
    error(file, new RuntimeException(message, e))

  /** This file was not formatted because it's excluded by project settings from
    * .scalafmt.conf.
    *
    * @param file
    *   the file path that was not formatted.
    */
  def excluded(file: Path): Unit

  /** This .scalafmt.conf file is missing the 'version' setting.
    *
    * @param config
    *   the .scalafmt.conf file.
    * @param defaultVersion
    *   the configured default Scalafmt version.
    */
  def missingVersion(config: Path, defaultVersion: String): Unit = {
    val message = String.format(
      "missing setting 'version'. To fix this problem, add the following line to .scalafmt.conf: 'version=%s'.",
      defaultVersion,
    )
    error(config, message)
  }

  /** The .scalafmt.conf file was parsed with the given Scalafmt version.
    */
  def parsedConfig(config: Path, scalafmtVersion: String): Unit

  /** Use {@link #downloadOutputStreamWriter} instead.
    */
  @deprecated
  def downloadWriter(): PrintWriter

  /** Use this writer for printing progress while downloading new Scalafmt
    * versions.
    */
  def downloadOutputStreamWriter(): OutputStreamWriter
}
