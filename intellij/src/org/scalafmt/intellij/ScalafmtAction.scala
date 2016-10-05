/*
 * Original:
 * https://github.com/thesamet/scalariform-intellij-plugin/blob/8e974a2c927db35f95b710b7498d5a5dba08de5e/src/com/thesamet/intellij/ScalariformFormatAction.scala
 */
package org.scalafmt.intellij

import java.io.File
import scala.collection.mutable
import scala.meta.parsers.ParseException

import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.CommandProcessor
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManager
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.ui.MessageType
import com.intellij.openapi.ui.popup.Balloon
import com.intellij.openapi.ui.popup.JBPopupFactory
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.openapi.wm.WindowManager
import com.intellij.ui.awt.RelativePoint
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.cli.Cli.Config
import org.scalafmt.cli.StyleCache
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps
import org.scalafmt.util.LoggerOps

case class FileDocument(file: VirtualFile, document: Document) {
  def isSbt: Boolean = file.getFileType.getName == "SBT"
  def isScala: Boolean = file.getFileType.getName == "Scala"
  def canFormat: Boolean = {
    isScala || isSbt
  }

}

object StyleChangedCache {
  val styleCache = mutable.Map.empty[String, ScalafmtConfig]
}

class ScalafmtAction extends AnAction {

  override def actionPerformed(event: AnActionEvent): Unit = {
    val style = getStyle(event)
    getCurrentFileDocument(event).filter(_.canFormat).foreach { fileDoc =>
      val source = fileDoc.document.getText()
      val runner = {
        if (fileDoc.isSbt)
          ScalafmtRunner.default.copy(dialect = scala.meta.dialects.Sbt0137)
        else ScalafmtRunner.default
      }
      Scalafmt.format(
        source,
        style = style.copy(runner = runner)
      ) match {
        case Formatted.Failure(e: ParseException) =>
          displayMessage(event,
                         "Parse error: " + e.getMessage,
                         MessageType.ERROR)
        case Formatted.Failure(e) =>
          displayMessage(event, e.getMessage.take(100), MessageType.ERROR)
        case Formatted.Success(formatted) =>
          if (source != formatted) {
            ApplicationManager.getApplication.runWriteAction(new Runnable {
              override def run(): Unit = {
                CommandProcessor
                  .getInstance()
                  .runUndoTransparentAction(new Runnable {
                    override def run(): Unit =
                      fileDoc.document.setText(formatted)
                  })
              }
            })
          }
      }
    }
  }

  private def getConfigFileInPath(path: String) = {
    Option(FileOps.getFile(path, ".scalafmt.conf")).collect {
      case file: File if file.isFile => file.getAbsolutePath
    }
  }

  def emitMigrateConfigWarning(event: AnActionEvent, configFile: File): Unit = {
    if (configFile.isFile) {
      displayMessage(
        event,
        "Ignoring configuration file '.scalafmt', please remove it. " +
          "Configuration is now read from '.scalafmt.conf' using HOCON syntax. " +
          "Run `scalafmt --migrate2hocon .scalafmt` from the the CLI to migrate your settings. " +
          "More details in changelog for 0.4  release.",
        MessageType.WARNING)
    }
  }

  private val homeDir = System.getProperty("user.home")

  private def getStyle(event: AnActionEvent): ScalafmtConfig = {

    val customStyle: Option[ScalafmtConfig] = for {
      project <- Option(event.getData(CommonDataKeys.PROJECT))
      _ = emitMigrateConfigWarning(event,
                                   new File(project.getBasePath, ".scalafmt"))
      localConfig = getConfigFileInPath(project.getBasePath)
      globalConfig = getConfigFileInPath(homeDir)
      configFile <- localConfig.orElse(globalConfig)
      config <- {
        StyleCache.getStyleForFileOrError(configFile) match {
          case Left(e) =>
            displayMessage(event,
                           "Failed to read .scalafmt.conf. " + e.getMessage,
                           MessageType.WARNING)
            None
          case Right(config) => Some(config)
        }
      }
    } yield {
      if (!StyleChangedCache.styleCache.get(configFile).contains(config)) {
        displayMessage(event,
                       "scalafmt picked up new style configuration",
                       MessageType.INFO)
        StyleChangedCache.styleCache.update(configFile, config)
      }
      config
    }
    customStyle.getOrElse(ScalafmtConfig.default)
  }

  private def getCurrentFileDocument(
      event: AnActionEvent): Option[FileDocument] = {
    for {
      project <- Option(event.getData(CommonDataKeys.PROJECT))
      editor <- Option(
        FileEditorManager.getInstance(project).getSelectedTextEditor)
      document <- Option(editor.getDocument)
      vfile <- Option(FileDocumentManager.getInstance().getFile(document))
    } yield FileDocument(vfile, document)
  }

  def displayMessage(event: AnActionEvent,
                     msg: String,
                     messageType: MessageType): Unit = {
    WindowManager.getInstance()
    val statusBar = WindowManager.getInstance().getStatusBar(event.getProject)
    JBPopupFactory
      .getInstance()
      .createHtmlTextBalloonBuilder(msg, messageType, null)
      .setFadeoutTime(5000)
      .createBalloon()
      .show(RelativePoint.getCenterOf(statusBar.getComponent),
            Balloon.Position.atRight)
  }
}
