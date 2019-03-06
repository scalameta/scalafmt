package org.scalafmt.intellij

import java.awt.{Component, FlowLayout}
import javax.swing._

import com.intellij.openapi.options.SearchableConfigurable
import com.intellij.openapi.project.Project

class SettingsGui(project: Project) extends SearchableConfigurable {
  override def getDisplayName: String = IdeaUtils.PluginName

  override def getId: String = "preference." + IdeaUtils.PluginName

  override def getHelpTopic: String = "preference." + IdeaUtils.PluginName

  override def enableSearch(s: String): Runnable = null

  private val formatOnSaveCheckBox = new JCheckBox("Format on file save")

  private val relativePathToConfigTextField =
    new JTextField(IdeaUtils.DefaultConfigPath, 25)

  override def createComponent(): JComponent = {
    val panel = new JPanel()
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS))

    val formatOnSavePanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    formatOnSavePanel.add(formatOnSaveCheckBox)
    formatOnSavePanel.setMaximumSize(formatOnSavePanel.getPreferredSize)
    formatOnSavePanel.setAlignmentX(Component.LEFT_ALIGNMENT)
    val pathToConfigPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    pathToConfigPanel.add(new JLabel("Relative path to scalafmt config: "))
    pathToConfigPanel.add(relativePathToConfigTextField)
    pathToConfigPanel.setMaximumSize(pathToConfigPanel.getPreferredSize)
    pathToConfigPanel.setAlignmentX(Component.LEFT_ALIGNMENT)

    panel.add(formatOnSavePanel)
    panel.add(pathToConfigPanel)
    panel
  }

  override def isModified: Boolean =
    formatOnSaveCheckBox.isSelected != IdeaSettings(project).formatOnSave ||
      relativePathToConfigTextField.getText !=
        IdeaSettings(project).relativePathToConfig

  override def disposeUIResources(): Unit = ()

  override def apply(): Unit = {
    IdeaSettings(project).formatOnSave = formatOnSaveCheckBox.isSelected
    IdeaSettings(project).relativePathToConfig =
      relativePathToConfigTextField.getText
  }

  override def reset(): Unit = {
    formatOnSaveCheckBox.setSelected(IdeaSettings(project).formatOnSave)
    relativePathToConfigTextField.setText(
      IdeaSettings(project).relativePathToConfig
    )
  }
}
