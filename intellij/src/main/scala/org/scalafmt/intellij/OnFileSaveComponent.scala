package org.scalafmt.intellij

import com.intellij.AppTopics
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.components.ApplicationComponent
import com.intellij.openapi.editor.Document
import com.intellij.openapi.fileEditor.FileDocumentManagerAdapter

class OnFileSaveComponent extends ApplicationComponent {

  def getComponentName = IdeaUtils.PluginName

  def initComponent() {
    val bus = ApplicationManager.getApplication.getMessageBus
    bus
      .connect()
      .subscribe(
        AppTopics.FILE_DOCUMENT_SYNC,
        new FileDocumentManagerAdapter {
          override def beforeDocumentSaving(document: Document) = {
            val fileDoc = FileDocument(document)
            if (fileDoc.project.exists(IdeaSettings(_).formatOnSave))
              fileDoc.format()
          }
        }
      )
  }

  def disposeComponent(): Unit = ()
}
