package org.scalafmt.intellij

import java.awt.FlowLayout
import javax.swing.{JCheckBox, JComponent, JPanel}

import com.intellij.openapi.options.SearchableConfigurable

class SettingsGui extends SearchableConfigurable {
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
    formatOnSaveCheckBox.isSelected != Settings().formatOnSave

  override def disposeUIResources(): Unit = ()

  override def apply(): Unit =
    Settings().formatOnSave = formatOnSaveCheckBox.isSelected

  override def reset(): Unit =
    formatOnSaveCheckBox.setSelected(Settings().formatOnSave)
}
