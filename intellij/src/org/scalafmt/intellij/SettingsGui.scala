package org.scalafmt.intellij

import java.awt.FlowLayout
import javax.swing._

import com.intellij.openapi.options.SearchableConfigurable
import com.intellij.openapi.project.Project

class SettingsGui(project: Project) extends SearchableConfigurable {
  override def getDisplayName: String = Utils.PluginName

  override def getId: String = "preference." + Utils.PluginName

  override def getHelpTopic: String = "preference." + Utils.PluginName

  override def enableSearch(s: String): Runnable = null

  private val formatOnSaveCheckBox = new JCheckBox("Format on file save")

  override def createComponent(): JComponent = {
    val panel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    panel.add(formatOnSaveCheckBox)
    panel
  }

  override def isModified: Boolean =
    formatOnSaveCheckBox.isSelected != Settings(project).formatOnSave

  override def disposeUIResources(): Unit = ()

  override def apply(): Unit =
    Settings(project).formatOnSave = formatOnSaveCheckBox.isSelected

  override def reset(): Unit =
    formatOnSaveCheckBox.setSelected(Settings(project).formatOnSave)
}
