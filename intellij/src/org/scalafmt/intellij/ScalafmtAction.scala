/*
 * Original:
 * https://github.com/thesamet/scalariform-intellij-plugin/blob/8e974a2c927db35f95b710b7498d5a5dba08de5e/src/com/thesamet/intellij/ScalariformFormatAction.scala
 */
package org.scalafmt.intellij

import java.io.File

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.vfs.VirtualFile
import org.scalafmt.ScalaFmt
import org.scalafmt.ScalaStyle
import org.scalafmt.util.FileOps

case class FileDocument(file: VirtualFile, document: Document) {
  def isScala: Boolean = file.getFileType.getName == "Scala"
}

class ScalafmtAction extends AnAction {

  override def actionPerformed(event: AnActionEvent): Unit = {
    val currentPath = new File(".").getAbsolutePath
    getCurrentFileDocument(event).filter(_.isScala).foreach { fileDoc =>
      val source = fileDoc.document.getText()
      val formatted = ScalaFmt.format(source)
      if (source != formatted) {
        ApplicationManager.getApplication.runWriteAction(new Runnable {
          override def run(): Unit = {
            CommandProcessor
              .getInstance()
              .runUndoTransparentAction(new Runnable {
                override def run(): Unit =
                  fileDoc.document
                    .setText(event.getProject.getBasePath + formatted)
              })
          }
        })
      }
    }
  }

  private def getStyle(event: AnActionEvent): ScalaStyle = {
    val customStyle = (for {
      project <- Option(event.getData(CommonDataKeys.PROJECT))
      configFile = FileOps.getFile(project.getBasePath, ".scalafmt")
          if (configFile.isFile)
      style <- org.scalafmt.Cli
        .parseConfigFile(FileOps.readFile(configFile))
        .style
    } yield style)
    customStyle.getOrElse(ScalaStyle.Default)
  }

  private def getCurrentFileDocument(
      event: AnActionEvent): Option[FileDocument] =
    for {
      project <- Option(event.getData(CommonDataKeys.PROJECT))
      editor <- Option(
          FileEditorManager.getInstance(project).getSelectedTextEditor)
      document <- Option(editor.getDocument)
      vfile <- Option(FileDocumentManager.getInstance().getFile(document))
    } yield FileDocument(vfile, document)
}
