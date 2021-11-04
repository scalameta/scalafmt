package org.scalafmt.interfaces

import java.nio.file.Path

/** A session based on a fixed configuration.
  */
trait ScalafmtSession {

  /** Format a single file with the given configuration.
    *
    * @param file
    *   relative or absolute path to the file being formatted. Used only for the
    *   path name, the file does not have to exist on disk.
    * @param code
    *   the text contents to format.
    * @return
    *   the formatted contents if formatting was successful, otherwise the
    *   original text contents.
    */
  def format(file: Path, code: String): String

  /** Whether the path matches the 'project.{excludeFilters,includeFilters}'
    * setting.
    */
  def matchesProjectFilters(file: Path): Boolean
}
