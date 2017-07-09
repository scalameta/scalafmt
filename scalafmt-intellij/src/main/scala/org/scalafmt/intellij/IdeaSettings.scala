package org.scalafmt.intellij

import scala.beans.BeanProperty

import com.intellij.openapi.components._
import com.intellij.openapi.project.Project
import com.intellij.util.xmlb.XmlSerializerUtil

@State(
  name = "ScalafmtSettings",
  storages = Array(new Storage(StoragePathMacros.WORKSPACE_FILE)))
class IdeaSettings extends PersistentStateComponent[IdeaSettings] {
  @BeanProperty
  var formatOnSave: Boolean = false

  override def loadState(config: IdeaSettings): Unit =
    XmlSerializerUtil.copyBean(config, this)

  override def getState: IdeaSettings = this
}

object IdeaSettings {
  def apply(project: Project) =
    ServiceManager.getService(project, classOf[IdeaSettings])
}
