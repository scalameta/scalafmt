/* Modified version of
https://github.com/sbt/sbt-scalariform/blob/61a0b7b75441b458e4ff3c6c30ed87d087a2e569/src/main/scala/com/typesafe/sbt/SbtScalariform.scala

Original licence:

Copyright 2011-2012 Typesafe Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */
package org.scalafmt.sbt

import scala.language.reflectiveCalls

import sbt.File
import sbt.FileFilter
import sbt.Keys.TaskStreams
import sbt.ProjectRef

import scala.util.control.NonFatal
import scala.collection.immutable.Seq

case class HasScalaFmt(reflective: ScalaFmtLike,
                       streams: TaskStreams,
                       sourceDirectories: Seq[File],
                       includeFilter: FileFilter,
                       excludeFilter: FileFilter,
                       ref: ProjectRef) {
  import sbt.{Future => _, _}

  def log(label: String, logger: Logger)(message: String)(count: String) =
    logger.info(message.format(count, label))

  val logFun = log(Reference.display(ref), streams.log) _

  val files =
    sourceDirectories.descendantsExcept(includeFilter, excludeFilter).get.toSet

  def writeFormattedContentsToFiles(): Unit = {
    val cache = streams.cacheDirectory / "scalafmt"
    handleFiles(files,
                cache,
                logFun("Formatting %s %s ..."),
                files => files.foreach(handleFile(writeFormatted)))
    handleFiles(files, cache, logFun("Reformatted %s %s."), _ => ())
  }

  def testProjectIsFormatted(): Unit = {
    files.foreach(handleFile(testFormatted))
  }

  private def testFormatted(result: FormatResult): Unit = {
    if (result.formattedContents != result.originalContents) {
      System.err.println(s"${result.file.getPath} is mis-formatted.")
      System.exit(1)
    }
  }

  private def writeFormatted(result: FormatResult): Unit = {
    if (result.formattedContents != result.originalContents) {
      IO.write(result.file, result.formattedContents)
    }
  }

  private case class FormatResult(
      file: File, originalContents: String, formattedContents: String)

  private def handleFile(callback: FormatResult => Unit)(file: File): Unit = {
    try {
      val contents = IO.read(file)
      val formatted = reflective.format(contents)
      callback(FormatResult(file, contents, formatted))
    } catch {
      case NonFatal(e) =>
        // Very unlikely, since NonFatal exceptions are caught by scalafmt.
        streams.log.warn(
            s"""Scalafmt error, please report to https://github.com/olafurpg/scalafmt/issues
               |  $file:
               |  ${e.getMessage}""".stripMargin)
    }
  }

  private def handleFiles(files: Set[File],
                          cache: File,
                          logFun: String => Unit,
                          updateFun: Set[File] => Unit): Set[File] = {

    def handleUpdate(in: ChangeReport[File], out: ChangeReport[File]) = {
      val files = in.modified -- in.removed
      import sbt._
      inc.Analysis.counted("Scala source", "", "s", files.size).foreach(logFun)
      updateFun(files)
      files
    }

    FileFunction.cached(cache)(FilesInfo.hash, FilesInfo.exists)(handleUpdate)(
        files)
  }
}
