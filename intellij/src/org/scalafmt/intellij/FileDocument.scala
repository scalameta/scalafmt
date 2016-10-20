package org.scalafmt.intellij

import scala.meta.parsers.ParseException

import com.intellij.notification.NotificationType
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.{Formatted, Scalafmt}

case class FileDocument(document: Document) {
  private val virtualFile = FileDocumentManager.getInstance().getFile(document)
  def isSbt: Boolean = virtualFile.getFileType.getName == "SBT"
  def isScala: Boolean = virtualFile.getFileType.getName == "Scala"
  def canFormat: Boolean = isScala || isSbt

  def format(): Unit = if (canFormat) {
    import Utils._
    val source = document.getText()
    val style = getStyle(projectForFile(virtualFile.getPath))
    val runner =
      if (isSbt)
        ScalafmtRunner.default.copy(dialect = scala.meta.dialects.Sbt0137)
      else ScalafmtRunner.default

    Scalafmt.format(
      source,
      style = style.copy(runner = runner)
    ) match {
      case Formatted.Failure(e: ParseException) =>
        displayMessage("Parse error: " + e.getMessage, NotificationType.ERROR)
      case Formatted.Failure(e) =>
        displayMessage(e.getMessage.take(100), NotificationType.ERROR)
      case Formatted.Success(formatted) =>
        if (source != formatted) {
          ApplicationManager.getApplication.runWriteAction(new Runnable {
            override def run(): Unit =
              CommandProcessor
                .getInstance()
                .runUndoTransparentAction(new Runnable {
                  override def run(): Unit =
                    document.setText(formatted)
                })
          })
        }
    }
  }
}
