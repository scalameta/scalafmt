package org.scalafmt.intellij

import java.io.File

import scala.collection.mutable

import com.intellij.notification.{Notification, NotificationType, Notifications}
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.{Project, ProjectManager}
import org.scalafmt.cli.StyleCache
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps

object Utils {

  val PluginName = "Scalafmt"

  def displayMessage(msg: String, notificationType: NotificationType): Unit =
    Notifications.Bus.notify(
      new Notification(PluginName,
                       PluginName,
                       msg,
                       NotificationType.INFORMATION))

  private def getConfigFileInPath(path: String) =
    Option(FileOps.getFile(path, ".scalafmt.conf")).collect {
      case file: File if file.isFile => file.getAbsolutePath
    }

  def projectForFile(file: String) = {
    val projects = ProjectManager.getInstance().getOpenProjects
    projects.find(bp => file.startsWith(bp.getBasePath))
  }

  private val homeDir = System.getProperty("user.home")
  private val styleCache = mutable.Map.empty[String, ScalafmtConfig]

  def getStyle(project: Option[Project]): ScalafmtConfig = {
    val localConfig = project.map(_.getBasePath).flatMap { basePath =>
      emitMigrateConfigWarning(new File(basePath, ".scalafmt"))
      getConfigFileInPath(basePath)
    }
    val globalConfig = getConfigFileInPath(homeDir)

    val customStyle: Option[ScalafmtConfig] = for {
      configFile <- localConfig.orElse(globalConfig)
      config <- StyleCache.getStyleForFileOrError(configFile) match {
        case Left(e) =>
          Utils.displayMessage(
            "Failed to read .scalafmt.conf. " + e.getMessage,
            NotificationType.WARNING)
          None
        case Right(config) => Some(config)
      }
    } yield {
      if (!styleCache.get(configFile).contains(config)) {
        Utils.displayMessage("scalafmt picked up new style configuration",
                             NotificationType.INFORMATION)
        styleCache.update(configFile, config)
      }
      config
    }
    customStyle.getOrElse(ScalafmtConfig.default)
  }

  private def emitMigrateConfigWarning(configFile: File): Unit =
    if (configFile.isFile) {
      Utils.displayMessage(
        "Ignoring configuration file '.scalafmt', please remove it. " +
          "Configuration is now read from '.scalafmt.conf' using HOCON syntax. " +
          "Run `scalafmt --migrate2hocon .scalafmt` from the the CLI to migrate your settings. " +
          "More details in changelog for 0.4  release.",
        NotificationType.WARNING)
    }

  def getCurrentFileDocument(event: AnActionEvent): Option[FileDocument] =
    for {
      project <- Option(event.getData(CommonDataKeys.PROJECT))
      editor <- Option(
        FileEditorManager.getInstance(project).getSelectedTextEditor)
      document <- Option(editor.getDocument)
    } yield FileDocument(document)
}
