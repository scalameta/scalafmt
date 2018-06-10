package org.scalafmt.intellij

import scala.meta.parsers.ParseException
import com.intellij.notification.NotificationType
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.{Formatted, Scalafmt}
import IdeaUtils._
import com.intellij.openapi.project.Project
import org.scalameta.logger

case class FileDocument(project: Option[Project], document: Document) {
  val virtualFile = FileDocumentManager.getInstance().getFile(document)
  def path = virtualFile.getPath
  def isSbt: Boolean =
    virtualFile.getFileType.getName.compareToIgnoreCase("SBT") == 0
  def isSc: Boolean =
    isScala && virtualFile.getExtension.compareToIgnoreCase(".SC") == 0
  def isScala: Boolean = virtualFile.getFileType.getName == "Scala"
  def canFormat: Boolean = isScala || isSbt

  def format(): Unit = if (canFormat) {
    val style = getStyle(project, virtualFile)
    val runner =
      if (isSbt || isSc)
        style.runner.forSbt
      else style.runner

    val source = document.getText()
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

object FileDocument {
  def apply(document: Document): FileDocument = {
    val project = projectForFile(
      FileDocumentManager.getInstance().getFile(document)
    )
    FileDocument(project, document)
  }
}
