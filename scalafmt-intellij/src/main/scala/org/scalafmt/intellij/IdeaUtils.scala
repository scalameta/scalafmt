package org.scalafmt.intellij

import java.io.File
import scala.collection.mutable
import scala.xml.Utility

import com.intellij.notification.{Notification, NotificationType, Notifications}
import com.intellij.openapi.actionSystem.{AnActionEvent, CommonDataKeys}
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.{Project, ProjectManager}
import metaconfig.Configured.NotOk
import metaconfig.Configured.Ok
import org.scalafmt.cli.StyleCache
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps

object IdeaUtils {

  val PluginName = "Scalafmt"

  val DefaultConfigPath = ".scalafmt.conf"

  def displayMessage(msg: String, notificationType: NotificationType): Unit =
    Notifications.Bus.notify(
      new Notification(
        PluginName,
        PluginName,
        Utility.escape(msg),
        notificationType))

  private def getConfigFileInPath(path: String, configRelativePath: String) =
    Option(FileOps.getFile(path, configRelativePath)).collect {
      case file: File if file.isFile => file.getAbsolutePath
    }

  def projectForFile(file: String) = {
    val projects = ProjectManager.getInstance().getOpenProjects
    projects.find(bp => file.startsWith(bp.getBasePath))
  }

  private val homeDir = System.getProperty("user.home")
  private val styleCache = mutable.Map.empty[String, ScalafmtConfig]

  def searchForConfig(project: Option[Project]): Option[String] = {
    val files = for {
      proj <- project.toList
      basePath = proj.getBasePath
      _ = emitMigrateConfigWarning(new File(basePath, ".scalafmt"))
      ideaSettings = IdeaSettings(proj)
      path <- Seq(basePath, homeDir)
      relPath <- Seq(ideaSettings.relativePathToConfig, DefaultConfigPath)
    } yield {
      getConfigFileInPath(path, relPath)
    }

    files.flatten.headOption
  }

  def getStyle(project: Option[Project]): ScalafmtConfig = {
    val customStyle: Option[ScalafmtConfig] = for {
      configFile <- searchForConfig(project)
      config <- StyleCache.getStyleForFileOrError(configFile) match {
        case NotOk(e) =>
          IdeaUtils.displayMessage(
            s"Failed to read $configFile. \n" + e.toString(),
            NotificationType.WARNING)
          None
        case Ok(config) => Some(config)
      }
    } yield {
      if (!styleCache.get(configFile).contains(config)) {
        IdeaUtils.displayMessage(
          "scalafmt picked up new style configuration",
          NotificationType.INFORMATION)
        styleCache.update(configFile, config)
      }
      config
    }
    customStyle.getOrElse(ScalafmtConfig.default)
  }

  private def emitMigrateConfigWarning(configFile: File): Unit =
    if (configFile.isFile) {
      IdeaUtils.displayMessage(
        "Ignoring configuration file '.scalafmt', please remove it. " +
          "Configuration is now read from '.scalafmt.conf' using HOCON syntax. " +
          "Run `scalafmt --migrate2hocon .scalafmt` from the the CLI to migrate your settings. " +
          "More details in changelog for 0.4  release.",
        NotificationType.WARNING
      )
    }

  def getCurrentFileDocument(event: AnActionEvent): Option[FileDocument] =
    for {
      project <- Option(event.getData(CommonDataKeys.PROJECT))
      editor <- Option(
        FileEditorManager.getInstance(project).getSelectedTextEditor)
      document <- Option(editor.getDocument)
    } yield FileDocument(document)
}
